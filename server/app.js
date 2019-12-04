const express = require("express");
const bodyParser = require("body-parser");
const fs = require("fs");
const childProcess = require("child_process");
const tmp = require("tmp");
const cors = require("cors");
const app = express();
const port = process.env.PORT || 8000;

app.use(bodyParser.json());

app.use(cors({ origin: "*" }));

app.get("/", (req, res) => {
  res.send("hello world");
});

app.post("/transpile", (req, res) => {
  let tmpInFile = tmp.fileSync();

  console.log(req.body.program);
  console.log(tmpInFile.name);
  fs.writeFileSync(tmpInFile.name, req.body.program + "\n");

  let result = childProcess.execSync(`../run ${tmpInFile.name}`);

  res.send({ compiled: result.toString() });
});

app.listen(port, () => {
  console.log(`listening on port ${port}`);
});
