const fs = require("fs/promises");
const path = require("path");

const prefix =
  "-- WARNING: The following is generated code. Edit with care!\n\n";

const code = [
  {
    typeName: "Accept",
    varName: "accept",
    value: "accept",
    attribute: true
  },
  {
    typeName: "AcceptCharset",
    varName: "acceptCharset",
    value: "accept-charset",
    attribute: true
  },
  {
    typeName: "Accesskey",
    varName: "accesskey",
    value: "accesskey",
    attribute: true
  },
  {
    typeName: "Action",
    varName: "action",
    value: "action",
    attribute: true
  },
  {
    typeName: "All",
    varName: "all",
    value: "all",
  },
  {
    typeName: "Alt",
    varName: "alt",
    value: "alt",
    attribute: true
  },
  {
    typeName: "Async",
    varName: "async",
    value: "async",
    attribute: true
  },
  {
    typeName: "Auto",
    varName: "auto",
    value: "auto",
  },
  {
    typeName: "Autocomplete",
    varName: "autocomplete",
    value: "autocomplete",
    attribute: true
  },
  {
    typeName: "Autofocus",
    varName: "autofocus",
    value: "autofocus",
    attribute: true
  },
  {
    typeName: "Autoplay",
    varName: "autoplay",
    value: "autoplay",
    attribute: true
  },
  {
    typeName: "BorderBox",
    varName: "borderBox",
    value: "border-box",
  },
  {
    typeName: "Both",
    varName: "both",
    value: "both",
  },
  {
    typeName: "Bottom",
    varName: "bottom",
    value: "bottom",
  },
  {
    typeName: "Center",
    varName: "center",
    value: "center",
  },
  {
    typeName: "Charset",
    varName: "charset",
    value: "charset",
    attribute: true
  },
  {
    typeName: "Checked",
    varName: "checked",
    value: "checked",
    attribute: true
  },
  {
    typeName: "Circle",
    varName: "circle",
    value: "circle",
  },
  {
    typeName: "Cite",
    varName: "cite",
    value: "cite",
    attribute: true
  },
  {
    typeName: "Class",
    varName: "class'",
    value: "class",
    attribute: true
  },
  {
    typeName: "Cols",
    varName: "cols",
    value: "cols",
    attribute: true
  },
  {
    typeName: "Colspan",
    varName: "colspan",
    value: "colspan",
    attribute: true
  },
  {
    typeName: "Content",
    varName: "content",
    value: "content",
    attribute: true
  },
  {
    typeName: "ContentBox",
    varName: "contentBox",
    value: "content-box",
  },
  {
    typeName: "Contenteditable",
    varName: "contenteditable",
    value: "contenteditable",
    attribute: true
  },
  {
    typeName: "Controls",
    varName: "controls",
    value: "controls",
    attribute: true
  },
  {
    typeName: "Coords",
    varName: "coords",
    value: "coords",
    attribute: true
  },
  {
    typeName: "Data",
    varName: "data'",
    value: "data",
    attribute: true
  },
  {
    typeName: "Datetime",
    varName: "datetime",
    value: "datetime",
    attribute: true
  },
  {
    typeName: "Default",
    varName: "default",
    value: "default",
    attribute: true
  },
  {
    typeName: "Defer",
    varName: "defer",
    value: "defer",
    attribute: true
  },
  {
    typeName: "Dir",
    varName: "dir",
    value: "dir",
    attribute: true
  },
  {
    typeName: "Dirname",
    varName: "dirname",
    value: "dirname",
    attribute: true
  },
  {
    typeName: "Disabled",
    varName: "disabled",
    value: "disabled",
    attribute: true
  },
  {
    typeName: "Download",
    varName: "download",
    value: "download",
    attribute: true
  },
  {
    typeName: "Draggable",
    varName: "draggable",
    value: "draggable",
    attribute: true
  },
  {
    typeName: "Enctype",
    varName: "enctype",
    value: "enctype",
    attribute: true
  },
  {
    typeName: "End",
    varName: "end",
    value: "end"
  },
  {
    typeName: "Fixed",
    varName: "fixed",
    value: "fixed",
  },
  {
    typeName: "For",
    varName: "for",
    value: "for",
    attribute: true
  },
  {
    typeName: "Form",
    varName: "form",
    value: "form",
    attribute: true
  },
  {
    typeName: "Formaction",
    varName: "formaction",
    value: "formaction",
    attribute: true
  },
  {
    typeName: "Headers",
    varName: "headers",
    value: "headers",
    attribute: true
  },
  {
    typeName: "Height",
    varName: "height",
    value: "height",
    attribute: true
  },
  {
    typeName: "Hidden",
    varName: "hidden",
    value: "hidden",
    attribute: true
  },
  {
    typeName: "High",
    varName: "high",
    value: "high",
    attribute: true
  },
  {
    typeName: "Href",
    varName: "href",
    value: "href",
    attribute: true
  },
  {
    typeName: "Hreflang",
    varName: "hreflang",
    value: "hreflang",
    attribute: true
  },
  {
    typeName: "HttpEquiv",
    varName: "httpEquiv",
    value: "http-equiv",
    attribute: true
  },
  {
    typeName: "Id",
    varName: "id",
    value: "id",
    attribute: true
  },
  {
    typeName: "Ismap",
    varName: "ismap",
    value: "ismap",
    attribute: true
  },
  {
    typeName: "Kind",
    varName: "kind",
    value: "kind",
    attribute: true
  },
  {
    typeName: "Label",
    varName: "label",
    value: "label",
    attribute: true
  },
  {
    typeName: "Lang",
    varName: "lang",
    value: "lang",
    attribute: true
  },
  {
    typeName: "Left",
    varName: "left",
    value: "left",
  },
  {
    typeName: "List'",
    varName: "list",
    value: "list",
    attribute: true
  },
  {
    typeName: "Loop",
    varName: "loop",
    value: "loop",
    attribute: true
  },
  {
    typeName: "Low",
    varName: "low",
    value: "low",
    attribute: true
  },
  {
    typeName: "Max",
    varName: "max",
    value: "max",
    attribute: true
  },
  {
    typeName: "Maxlength",
    varName: "maxlength",
    value: "maxlength",
    attribute: true
  },
  {
    typeName: "Media",
    varName: "media'",
    value: "media",
    attribute: true
  },
  {
    typeName: "Method",
    varName: "method",
    value: "method",
    attribute: true
  },
  {
    typeName: "Min",
    varName: "min",
    value: "min",
    attribute: true
  },
  {
    typeName: "Multiple",
    varName: "multiple",
    value: "multiple",
    attribute: true
  },
  {
    typeName: "Muted",
    varName: "muted",
    value: "muted",
    attribute: true
  },
  {
    typeName: "Name",
    varName: "name",
    value: "name",
    attribute: true
  },
  {
    typeName: "None",
    varName: "none",
    value: "none",
  },
  {
    typeName: "Normal",
    varName: "normal",
    value: "normal",
  },
  {
    typeName: "Novalidate",
    varName: "novalidate",
    value: "novalidate",
    attribute: true
  },
  {
    typeName: "Onabort",
    varName: "onabort",
    value: "onabort",
    attribute: true
  },
  {
    typeName: "Onafterprint",
    varName: "onafterprint",
    value: "onafterprint",
    attribute: true
  },
  {
    typeName: "Onbeforeprint",
    varName: "onbeforeprint",
    value: "onbeforeprint",
    attribute: true
  },
  {
    typeName: "Onbeforeunload",
    varName: "onbeforeunload",
    value: "onbeforeunload",
    attribute: true
  },
  {
    typeName: "Onblur",
    varName: "onblur",
    value: "onblur",
    attribute: true
  },
  {
    typeName: "Oncanplay",
    varName: "oncanplay",
    value: "oncanplay",
    attribute: true
  },
  {
    typeName: "Oncanplaythrough",
    varName: "oncanplaythrough",
    value: "oncanplaythrough",
    attribute: true
  },
  {
    typeName: "Onchange",
    varName: "onchange",
    value: "onchange",
    attribute: true
  },
  {
    typeName: "Onclick",
    varName: "onclick",
    value: "onclick",
    attribute: true
  },
  {
    typeName: "Oncontextmenu",
    varName: "oncontextmenu",
    value: "oncontextmenu",
    attribute: true
  },
  {
    typeName: "Oncopy",
    varName: "oncopy",
    value: "oncopy",
    attribute: true
  },
  {
    typeName: "Oncuechange",
    varName: "oncuechange",
    value: "oncuechange",
    attribute: true
  },
  {
    typeName: "Oncut",
    varName: "oncut",
    value: "oncut",
    attribute: true
  },
  {
    typeName: "Ondblclick",
    varName: "ondblclick",
    value: "ondblclick",
    attribute: true
  },
  {
    typeName: "Ondrag",
    varName: "ondrag",
    value: "ondrag",
    attribute: true
  },
  {
    typeName: "Ondragend",
    varName: "ondragend",
    value: "ondragend",
    attribute: true
  },
  {
    typeName: "Ondragenter",
    varName: "ondragenter",
    value: "ondragenter",
    attribute: true
  },
  {
    typeName: "Ondragleave",
    varName: "ondragleave",
    value: "ondragleave",
    attribute: true
  },
  {
    typeName: "Ondragover",
    varName: "ondragover",
    value: "ondragover",
    attribute: true
  },
  {
    typeName: "Ondragstart",
    varName: "ondragstart",
    value: "ondragstart",
    attribute: true
  },
  {
    typeName: "Ondrop",
    varName: "ondrop",
    value: "ondrop",
    attribute: true
  },
  {
    typeName: "Ondurationchange",
    varName: "ondurationchange",
    value: "ondurationchange",
    attribute: true
  },
  {
    typeName: "Onemptied",
    varName: "onemptied",
    value: "onemptied",
    attribute: true
  },
  {
    typeName: "Onended",
    varName: "onended",
    value: "onended",
    attribute: true
  },
  {
    typeName: "Onerror",
    varName: "onerror",
    value: "onerror",
    attribute: true
  },
  {
    typeName: "Onfocus",
    varName: "onfocus",
    value: "onfocus",
    attribute: true
  },
  {
    typeName: "Onhashchange",
    varName: "onhashchange",
    value: "onhashchange",
    attribute: true
  },
  {
    typeName: "Oninput",
    varName: "oninput",
    value: "oninput",
    attribute: true
  },
  {
    typeName: "Oninvalid",
    varName: "oninvalid",
    value: "oninvalid",
    attribute: true
  },
  {
    typeName: "Onkeydown",
    varName: "onkeydown",
    value: "onkeydown",
    attribute: true
  },
  {
    typeName: "Onkeypress",
    varName: "onkeypress",
    value: "onkeypress",
    attribute: true
  },
  {
    typeName: "Onkeyup",
    varName: "onkeyup",
    value: "onkeyup",
    attribute: true
  },
  {
    typeName: "Onload",
    varName: "onload",
    value: "onload",
    attribute: true
  },
  {
    typeName: "Onloadeddata",
    varName: "onloadeddata",
    value: "onloadeddata",
    attribute: true
  },
  {
    typeName: "Onloadedmetadata",
    varName: "onloadedmetadata",
    value: "onloadedmetadata",
    attribute: true
  },
  {
    typeName: "Onloadstart",
    varName: "onloadstart",
    value: "onloadstart",
    attribute: true
  },
  {
    typeName: "Onmousedown",
    varName: "onmousedown",
    value: "onmousedown",
    attribute: true
  },
  {
    typeName: "Onmousemove",
    varName: "onmousemove",
    value: "onmousemove",
    attribute: true
  },
  {
    typeName: "Onmouseout",
    varName: "onmouseout",
    value: "onmouseout",
    attribute: true
  },
  {
    typeName: "Onmouseover",
    varName: "onmouseover",
    value: "onmouseover",
    attribute: true
  },
  {
    typeName: "Onmouseup",
    varName: "onmouseup",
    value: "onmouseup",
    attribute: true
  },
  {
    typeName: "Onmousewheel",
    varName: "onmousewheel",
    value: "onmousewheel",
    attribute: true
  },
  {
    typeName: "Onoffline",
    varName: "onoffline",
    value: "onoffline",
    attribute: true
  },
  {
    typeName: "Ononline",
    varName: "ononline",
    value: "ononline",
    attribute: true
  },
  {
    typeName: "Onpagehide",
    varName: "onpagehide",
    value: "onpagehide",
    attribute: true
  },
  {
    typeName: "Onpageshow",
    varName: "onpageshow",
    value: "onpageshow",
    attribute: true
  },
  {
    typeName: "Onpaste",
    varName: "onpaste",
    value: "onpaste",
    attribute: true
  },
  {
    typeName: "Onpause",
    varName: "onpause",
    value: "onpause",
    attribute: true
  },
  {
    typeName: "Onplay",
    varName: "onplay",
    value: "onplay",
    attribute: true
  },
  {
    typeName: "Onplaying",
    varName: "onplaying",
    value: "onplaying",
    attribute: true
  },
  {
    typeName: "Onpopstate",
    varName: "onpopstate",
    value: "onpopstate",
    attribute: true
  },
  {
    typeName: "Onprogress",
    varName: "onprogress",
    value: "onprogress",
    attribute: true
  },
  {
    typeName: "Onratechange",
    varName: "onratechange",
    value: "onratechange",
    attribute: true
  },
  {
    typeName: "Onreset",
    varName: "onreset",
    value: "onreset",
    attribute: true
  },
  {
    typeName: "Onresize",
    varName: "onresize",
    value: "onresize",
    attribute: true
  },
  {
    typeName: "Onscroll",
    varName: "onscroll",
    value: "onscroll",
    attribute: true
  },
  {
    typeName: "Onsearch",
    varName: "onsearch",
    value: "onsearch",
    attribute: true
  },
  {
    typeName: "Onseeked",
    varName: "onseeked",
    value: "onseeked",
    attribute: true
  },
  {
    typeName: "Onseeking",
    varName: "onseeking",
    value: "onseeking",
    attribute: true
  },
  {
    typeName: "Onselect",
    varName: "onselect",
    value: "onselect",
    attribute: true
  },
  {
    typeName: "Onstalled",
    varName: "onstalled",
    value: "onstalled",
    attribute: true
  },
  {
    typeName: "Onstorage",
    varName: "onstorage",
    value: "onstorage",
    attribute: true
  },
  {
    typeName: "Onsubmit",
    varName: "onsubmit",
    value: "onsubmit",
    attribute: true
  },
  {
    typeName: "Onsuspend",
    varName: "onsuspend",
    value: "onsuspend",
    attribute: true
  },
  {
    typeName: "Ontimeupdate",
    varName: "ontimeupdate",
    value: "ontimeupdate",
    attribute: true
  },
  {
    typeName: "Ontoggle",
    varName: "ontoggle",
    value: "ontoggle",
    attribute: true
  },
  {
    typeName: "Onunload",
    varName: "onunload",
    value: "onunload",
    attribute: true
  },
  {
    typeName: "Onvolumechange",
    varName: "onvolumechange",
    value: "onvolumechange",
    attribute: true
  },
  {
    typeName: "Onwaiting",
    varName: "onwaiting",
    value: "onwaiting",
    attribute: true
  },
  {
    typeName: "Onwheel",
    varName: "onwheel",
    value: "onwheel",
    attribute: true
  },
  {
    typeName: "Open",
    varName: "open",
    value: "open",
    attribute: true
  },
  {
    typeName: "Optimum",
    varName: "optimum",
    value: "optimum",
    attribute: true
  },
  {
    typeName: "Pattern",
    varName: "pattern",
    value: "pattern",
    attribute: true
  },
  {
    typeName: "Placeholder",
    varName: "placeholder",
    value: "placeholder",
    attribute: true
  },
  {
    typeName: "Poster",
    varName: "poster",
    value: "poster",
    attribute: true
  },
  {
    typeName: "Preload",
    varName: "preload",
    value: "preload",
    attribute: true
  },
  {
    typeName: "Readonly",
    varName: "readonly",
    value: "readonly",
    attribute: true
  },
  {
    typeName: "Rel",
    varName: "rel",
    value: "rel",
    attribute: true
  },
  {
    typeName: "Required",
    varName: "required",
    value: "required",
    attribute: true
  },
  {
    typeName: "Reversed",
    varName: "reversed",
    value: "reversed",
    attribute: true
  },
  {
    typeName: "Right",
    varName: "right",
    value: "right",
  },
  {
    typeName: "Rows",
    varName: "rows",
    value: "rows",
    attribute: true
  },
  {
    typeName: "Rowspan",
    varName: "rowspan",
    value: "rowspan",
    attribute: true
  },
  {
    typeName: "Sandbox",
    varName: "sandbox",
    value: "sandbox",
    attribute: true
  },
  {
    typeName: "Scope",
    varName: "scope",
    value: "scope",
    attribute: true
  },
  {
    typeName: "Scroll",
    varName: "scroll",
    value: "scroll",
  },
  {
    typeName: "Selected",
    varName: "selected",
    value: "selected",
    attribute: true
  },
  {
    typeName: "Shape",
    varName: "shape",
    value: "shape",
    attribute: true
  },
  {
    typeName: "Size",
    varName: "size",
    value: "size",
    attribute: true
  },
  {
    typeName: "Sizes",
    varName: "sizes",
    value: "sizes",
    attribute: true
  },
  {
    typeName: "Span",
    varName: "span",
    value: "span",
    attribute: true
  },
  {
    typeName: "Spellcheck",
    varName: "spellcheck",
    value: "spellcheck",
    attribute: true
  },
  {
    typeName: "Src",
    varName: "src",
    value: "src",
    attribute: true
  },
  {
    typeName: "Srcdoc",
    varName: "srcdoc",
    value: "srcdoc",
    attribute: true
  },
  {
    typeName: "Srclang",
    varName: "srclang",
    value: "srclang",
    attribute: true
  },
  {
    typeName: "Srcset",
    varName: "srcset",
    value: "srcset",
    attribute: true
  },
  {
    typeName: "Start",
    varName: "start",
    value: "start",
    attribute: true
  },
  {
    typeName: "Step",
    varName: "step",
    value: "step",
    attribute: true
  },
  {
    typeName: "Style",
    varName: "style",
    value: "style",
    attribute: true
  },
  {
    typeName: "Tabindex",
    varName: "tabindex",
    value: "tabindex",
    attribute: true
  },
  {
    typeName: "Target",
    varName: "target",
    value: "target",
    attribute: true
  },
  {
    typeName: "Title",
    varName: "title",
    value: "title",
    attribute: true
  },
  {
    typeName: "Top",
    varName: "top",
    value: "top",
  },
  {
    typeName: "Translate",
    varName: "translate'",
    value: "translate",
    attribute: true
  },
  {
    typeName: "Type'",
    varName: "type'",
    value: "type",
    attribute: true
  },
  {
    typeName: "Usemap",
    varName: "usemap",
    value: "usemap",
    attribute: true
  },
  {
    typeName: "Value",
    varName: "value",
    value: "value",
    attribute: true
  },
  {
    typeName: "Width",
    varName: "width",
    value: "width",
    attribute: true
  },
  {
    typeName: "Wrap",
    varName: "wrap",
    value: "wrap",
    attribute: true
  }
]
  .map(({ typeName, varName, value, attribute }) => [
    `data ${typeName} = ${typeName}`,
    `instance ToVal ${typeName} where val _ = val "${value}"`,
    `${varName} = ${typeName} :: ${typeName}`,
    ...attribute ? [`instance IsAttribute ${typeName}`] : [],
  ]
  .join("\n"))
  .join("\n\n");

async function main() {
  const filePath = path.join(__dirname, "..", "src", "PSCSS.purs");
  const current = await fs.readFile(filePath, "utf8");
  const update = current.substring(0, current.indexOf(prefix)) + prefix + code + "\n";
  await fs.writeFile(filePath, update);
}

main().catch(err => console.error(err));
