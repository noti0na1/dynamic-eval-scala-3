#!/usr/bin/env python3
"""
Generate a typed Scala facade for one tau2-bench domain's tools.

Instead of the agent calling tools generically with raw JSON, we expose one
ordinary in-scope Scala function per domain tool, with meaningful camelCase
names and fully typed parameters — the shape the Lacuna `agent` primitive is
built around. Nested object parameters (tau2 pydantic models referenced via
JSON-schema `$defs`/`$ref`, e.g. airline FlightInfo / Passenger / Payment) are
emitted as Scala `case class`es, so the facade never exposes `ujson.Value`.

Each generated function builds the tool's JSON arguments from its typed params
and dispatches through `Tau.callTool` (the agent never sees that). The file also
emits `val domainToolsDoc: String` — the data-type + signature list shown in the
agent prompt.

Usage:  python gen_tools.py --domain retail > facades/Retail.scala
The output is pure Scala on stdout (logs go to stderr), so redirect to a file.
"""

import argparse
import re
import sys

from loguru import logger

logger.remove()  # keep stdout clean — only Scala goes there

from tau2.registry import registry  # noqa: E402
from tau2.gym.gym_agent import GymAgent  # noqa: E402

TYPE_MAP = {"string": "String", "integer": "Int", "number": "Double", "boolean": "Boolean"}


def camel(s: str) -> str:
    parts = s.split("_")
    return parts[0] + "".join(p[:1].upper() + p[1:] for p in parts[1:])


def ref_name(sch: dict):
    """The $defs model name a (param or items) schema points at, if any —
    directly via $ref or inside an anyOf."""
    if not isinstance(sch, dict):
        return None
    if "$ref" in sch:
        return sch["$ref"].split("/")[-1]
    for alt in sch.get("anyOf", []):
        if isinstance(alt, dict) and "$ref" in alt:
            return alt["$ref"].split("/")[-1]
    return None


def scala_type(ps: dict) -> str:
    m = ref_name(ps)
    if m:
        return m
    t = ps.get("type")
    if t == "array":
        items = ps.get("items") or {}
        im = ref_name(items)
        if im:
            return f"List[{im}]"
        return f"List[{TYPE_MAP.get(items.get('type'), 'ujson.Value')}]"
    return TYPE_MAP.get(t, "ujson.Value")


def obj_to_json(var: str, model: dict) -> str:
    """A ujson.Obj(...) expression converting case-class value `var` to JSON."""
    parts = []
    for jn, fs in (model.get("properties") or {}).items():
        parts.append(f'"{jn}" -> {conv_expr(f"{var}.{camel(jn)}", fs)}')
    return f"ujson.Obj({', '.join(parts)})"


def conv_expr(var: str, ps: dict, models: dict | None = None) -> str:
    """Scala expression converting `var` (of scala_type(ps)) to a ujson.Value."""
    models = models if models is not None else MODELS
    m = ref_name(ps)
    if m:
        return obj_to_json(var, models[m])
    t = ps.get("type")
    if t == "string":
        return f"ujson.Str({var})"
    if t in ("integer", "number"):
        return f"ujson.Num({var})"
    if t == "boolean":
        return f"ujson.Bool({var})"
    if t == "array":
        items = ps.get("items") or {}
        im = ref_name(items)
        if im:
            return f"ujson.Arr({var}.map(x => {obj_to_json('x', models[im])})*)"
        it = items.get("type")
        if it == "string":
            return f"ujson.Arr({var}.map(ujson.Str(_))*)"
        if it in ("integer", "number"):
            return f"ujson.Arr({var}.map(x => ujson.Num(x))*)"
        if it == "boolean":
            return f"ujson.Arr({var}.map(ujson.Bool(_))*)"
        return f"ujson.Arr({var}*)"
    return var  # last-resort (should not happen for committed domains)


MODELS: dict = {}  # name -> JSON-schema of a $defs model (set per domain in gen)


def get_schemas(domain: str):
    env = registry.get_env_constructor(domain)(solo_mode=False)
    agent = GymAgent(tools=list(env.get_tools()), domain_policy=env.get_policy())
    return [t.openai_schema for t in agent.tools]


def example_value(fs: dict) -> str:
    """A plausible Scala literal for one case-class field — an enum default, an
    example mined from the field description ('such as 'X''), or a typed
    placeholder."""
    t = fs.get("type")
    if fs.get("enum") and t == "string":
        return f'"{fs["enum"][0]}"'
    if t == "string":
        m = re.search(r"(?:such as|e\.g\.?,?|like)\s+'([^']+)'", fs.get("description") or "")
        return f'"{m.group(1) if m else "..."}"'
    if t == "integer":
        return "0"
    if t == "number":
        return "0.0"
    if t == "boolean":
        return "false"
    return '"..."'


def example_ctor(name: str, model: dict) -> str:
    req = model.get("required") or []
    vals = []
    for jn, fs in (model.get("properties") or {}).items():
        v = example_value(fs)
        vals.append(v if jn in req else f"Some({v})")
    return f"{name}({', '.join(vals)})"


def case_class_def(name: str, model: dict) -> str:
    req = model.get("required") or []
    fields = []
    for jn, fs in (model.get("properties") or {}).items():
        st = scala_type(fs)
        if jn in req:
            fields.append(f"{camel(jn)}: {st}")
        else:
            fields.append(f"{camel(jn)}: Option[{st}] = None")
    return f"case class {name}({', '.join(fields)})"


def gen(domain: str) -> str:
    global MODELS
    schemas = get_schemas(domain)

    # Collect all nested models ($defs) referenced by the domain's tools.
    MODELS = {}
    for s in schemas:
        for n, m in (s["function"].get("parameters") or {}).get("$defs", {}).items():
            MODELS.setdefault(n, m)

    out = [
        f"// Typed Scala facade for tau2-bench domain '{domain}' — the FIXED, committed",
        "// tool surface the agent calls. Generated by gen_tools.py; regenerate the",
        "// committed facades after a tau2 upgrade with:  ./regen_facades.sh",
        "// (which overwrites these files). Each function dispatches to the in-scope",
        "// tau2 tool via Tau.callTool.",
        "",
        # Lets the bench driver assert at runtime that the co-loaded facade
        # matches the domain being run (so the prompt never lists the wrong tools).
        f'val facadeDomain: String = "{domain}"',
        "",
    ]
    type_doc = []
    if MODELS:
        out.append("// --- data types for structured tool parameters ---")
        for n, m in MODELS.items():
            out.append(case_class_def(n, m))
            type_doc.append(f"- {case_class_def(n, m)}")
        out.append("")

    doc = []
    for s in schemas:
        f = s["function"]
        name = f["name"]
        fname = camel(name)
        desc = " ".join((f.get("description") or "").split())
        params = (f.get("parameters") or {}).get("properties") or {}
        req = (f.get("parameters") or {}).get("required") or []
        # required params first, then optionals (so defaults come last)
        ordered = [p for p in params if p in req] + [p for p in params if p not in req]

        sig_parts, body = [], []
        for pn in ordered:
            ps = params[pn]
            cn, st = camel(pn), scala_type(ps)
            if pn in req:
                sig_parts.append(f"{cn}: {st}")
                body.append(f'  args("{pn}") = {conv_expr(cn, ps)}')
            else:
                sig_parts.append(f"{cn}: Option[{st}] = None")
                body.append(f'  {cn}.foreach(v => args("{pn}") = {conv_expr("v", ps)})')

        sig = f"{fname}({', '.join(sig_parts)}): String"
        doc.append(f"- {sig}" + (f"  — {desc}" if desc else ""))

        if desc:
            out.append(f"/** {desc} */")
        if not params:
            out.append(f'def {fname}(): String = Tau.callTool("{name}")')
        else:
            out.append(f"def {fname}({', '.join(sig_parts)}): String =")
            out.append("  val args = ujson.Obj()")
            out.extend(body)
            out.append(f'  Tau.callTool("{name}", args)')
        out.append("")

    out.append("/** Data types + tool signatures for the agent prompt. */")
    out.append("val domainToolsDoc: String =")
    doc_body = ""
    if type_doc:
        doc_body += "DATA TYPES (construct these for the typed params below):\n"
        doc_body += "\n".join(type_doc) + "\n\n"
        doc_body += ("EXAMPLE VALUES (construct case classes positionally; wrap "
                     "collections in List(...)):\n")
        doc_body += "\n".join(f"  {example_ctor(n, m)}" for n, m in MODELS.items())
        doc_body += ("\n  // e.g. a tool param `flights: List[FlightInfo]` is passed "
                     "as List(FlightInfo(...), FlightInfo(...))\n\nTOOL FUNCTIONS:\n")
    doc_body += "\n".join(doc)
    out.append('  """' + doc_body + '"""')
    out.append("")
    return "\n".join(out)


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("--domain", required=True)
    args = ap.parse_args()
    sys.stdout.write(gen(args.domain))
