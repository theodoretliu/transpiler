import React, { useState } from "react";
import Editor from "react-simple-code-editor";
import { highlight, languages } from "prismjs/components/prism-core";
import "prismjs/components/prism-clike";
import "prismjs/components/prism-javascript";
import "prismjs/components/prism-python";
import "prismjs/components/prism-css";
import "prismjs/components/prism-ocaml";
import "prismjs/themes/prism.css";

import "./App.css";

import _ from "lodash";

async function makeRequest(s, setCompiledCode) {
  let res = await fetch(process.env.REACT_APP_API_URL + "/transpile", {
    method: "POST",
    body: JSON.stringify({
      program: s
    }),
    headers: {
      "content-type": "application/json"
    }
  });

  res = await res.json();

  setCompiledCode(res.compiled);
}

const debouncedMakeRequest = _.debounce(makeRequest, 100);

function App() {
  const [code, setCode] = useState(`x = 11`);
  const [compiledCode, setCompiledCode] = useState(`let x = 11`);

  function setAndRequest(code) {
    setCode(code);
    debouncedMakeRequest(code, setCompiledCode);
  }

  return (
    <div className="container">
      <h1 className="python">Python</h1>
      <Editor
        className="left"
        value={code}
        onValueChange={setAndRequest}
        highlight={code => highlight(code, languages.python, "python")}
        padding={10}
        tabSize={4}
        style={{
          fontFamily: "Monaco",
          fontSize: 16
        }}
      />
      <h1 className="ocaml">OCaml</h1>
      <pre className="right">
        <code
          className="language-ocaml"
          dangerouslySetInnerHTML={{
            __html: highlight(compiledCode, languages.python, "ocaml")
          }}
        />
      </pre>
    </div>
  );
}

export default App;
