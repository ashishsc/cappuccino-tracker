import express = require("express");
import { Client } from "pg";

const app = express();
const client = new Client();
await client.connect();

app.get("/", (req, res) => res.send("Hello World!"));

app.listen(3000, () => console.log("Example app listening on port 3000!"));
