hljs.registerLanguage("sql",
  function(e){
    var r="([a-zA-Z]|\\.[a-zA-Z.])[a-zA-Z0-9._]*";
    return{
      c:[e.HCM,
      {b:r,l:r,k:
        {keyword:"SELECT FROM WHERE AND AS GROUP ORDER BY CREATE TABLE",
        literal:"NULL NA TRUE FALSE T F",
          keyname : "PRIMARY KEY",
          keyfunc: "COUNT SUM DESC IS NOT",
          models: "CHAR TEXT DATE INT BOOLEAN VARCHAR",
          envfunc: "DESC AS"
        }, 
        r:0},
      {cN:"number",b:"0[xX][0-9a-fA-F]+[Li]?\\b",r:0},
      {cN:"number",b:"\\d+(?:[eE][+\\-]?\\d*)?L\\b",r:0},
      {cN:"number",b:"\\d+\\.(?!\\d)(?:i\\b)?",r:0},
      {cN:"number",b:"\\d+(?:\\.\\d*)?(?:[eE][+\\-]?\\d*)?i?\\b",r:0},
      {cN:"number",b:"\\.\\d+(?:[eE][+\\-]?\\d*)?i?\\b",r:0},
      {cN:"pipe",b:"%>%",r:0},
      {b:"`",e:"`",r:0},
      {cN:"string",c:[e.BE],v:[{b:'"',e:'"'},{b:"'",e:"'"}]},
      {cN: "keyword", b: /(^|\s*)(:::?|\.)\w+(?=\(|$)/}, ]}});


hljs.initHighlightingOnLoad();