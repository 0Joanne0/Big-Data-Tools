/**
 * Validation rapide des dates de publication.
 *
 * Usage:
 *   node validate_publish_date.js [path/to/data_jobs.csv]
 *
 * - Vérifie qu'il n'y a pas de Publish_Date dans le futur (par rapport à aujourd'hui)
 * - Affiche quelques exemples si c'est le cas
 *
 * Note: ce script n'est pas requis par l'app Shiny, c'est un garde-fou pour la data.
 */

const fs = require("fs");

const inputPath = process.argv[2] || (fs.existsSync("www/data_jobs.csv") ? "www/data_jobs.csv" : "data_jobs.csv");

if (!fs.existsSync(inputPath)) {
  console.error(`File not found: ${inputPath}`);
  process.exit(2);
}

const text = fs.readFileSync(inputPath, "utf8");

function isMissingTxt(x) {
  if (x == null) return true;
  const s = String(x).trim().toLowerCase();
  return (
    !s ||
    [
      "non spécifié",
      "non specifie",
      "non spécifie",
      "non déterminé",
      "non determine",
      "non determinée",
      "indéterminée",
      "indeterminee",
      "non déterminée",
      "non determinee",
    ].includes(s)
  );
}

function parsePublishDate(x) {
  if (x == null) return null;
  let s = String(x).trim();
  if (isMissingTxt(s)) return null;
  s = s.replace(/T.*$/, "");
  s = s.replace(/\s+.*$/, "");

  let m;
  // dd/mm/yyyy
  if ((m = s.match(/^(\d{1,2})\/(\d{1,2})\/(\d{4})$/))) {
    const dd = Number(m[1]);
    const mm = Number(m[2]);
    const yyyy = Number(m[3]);
    const d = new Date(yyyy, mm - 1, dd);
    if (d.getFullYear() === yyyy && d.getMonth() === mm - 1 && d.getDate() === dd) return d;
  }
  // yyyy-mm-dd
  if ((m = s.match(/^(\d{4})-(\d{1,2})-(\d{1,2})$/))) {
    const yyyy = Number(m[1]);
    const mm = Number(m[2]);
    const dd = Number(m[3]);
    const d = new Date(yyyy, mm - 1, dd);
    if (d.getFullYear() === yyyy && d.getMonth() === mm - 1 && d.getDate() === dd) return d;
  }
  // dd-mm-yyyy
  if ((m = s.match(/^(\d{1,2})-(\d{1,2})-(\d{4})$/))) {
    const dd = Number(m[1]);
    const mm = Number(m[2]);
    const yyyy = Number(m[3]);
    const d = new Date(yyyy, mm - 1, dd);
    if (d.getFullYear() === yyyy && d.getMonth() === mm - 1 && d.getDate() === dd) return d;
  }
  // yyyy/mm/dd
  if ((m = s.match(/^(\d{4})\/(\d{1,2})\/(\d{1,2})$/))) {
    const yyyy = Number(m[1]);
    const mm = Number(m[2]);
    const dd = Number(m[3]);
    const d = new Date(yyyy, mm - 1, dd);
    if (d.getFullYear() === yyyy && d.getMonth() === mm - 1 && d.getDate() === dd) return d;
  }

  // Excel numeric days
  if (/^\d+(?:\.\d+)?$/.test(s)) {
    const n = Number(s);
    if (Number.isFinite(n)) {
      const origin = new Date(Date.UTC(1899, 11, 30)); // 1899-12-30
      const dt = new Date(origin.getTime() + Math.round(n) * 86400000);
      return new Date(dt.getUTCFullYear(), dt.getUTCMonth(), dt.getUTCDate());
    }
  }

  return null;
}

function daysDiff(today, d) {
  const t0 = new Date(today.getFullYear(), today.getMonth(), today.getDate());
  const d0 = new Date(d.getFullYear(), d.getMonth(), d.getDate());
  return Math.trunc((t0 - d0) / 86400000);
}

// --- minimal CSV parser (handles quotes & newlines) ---
let header = null;
let publishIdx = -1;

let field = "";
let record = [];
let inQuotes = false;

function endField() {
  record.push(field);
  field = "";
}

const today = new Date();
let rows = 0;
let parsed = 0;
let missing = 0;
let future = 0;
let todayCnt = 0;
let past = 0;

const futureSamples = [];

function processRecord(rec) {
  if (!header) {
    header = rec;
    publishIdx = header.indexOf("Publish_Date");
    if (publishIdx < 0) {
      console.error("Publish_Date column not found in header");
      process.exit(2);
    }
    return;
  }

  rows++;
  const raw = rec[publishIdx];
  const dt = parsePublishDate(raw);
  if (!dt) {
    missing++;
    return;
  }
  parsed++;
  const d = daysDiff(today, dt);
  if (d < 0) {
    future++;
    if (futureSamples.length < 8) futureSamples.push({ raw, parsed: dt.toISOString().slice(0, 10), diffDays: d });
  } else if (d === 0) {
    todayCnt++;
  } else {
    past++;
  }
}

for (let i = 0; i < text.length; i++) {
  const ch = text[i];

  if (ch === '"') {
    if (inQuotes && text[i + 1] === '"') {
      field += '"';
      i++;
    } else {
      inQuotes = !inQuotes;
    }
    continue;
  }

  if (!inQuotes && ch === ";") {
    endField();
    continue;
  }

  if (!inQuotes && (ch === "\n" || ch === "\r")) {
    if (ch === "\r" && text[i + 1] === "\n") i++;
    endField();
    processRecord(record);
    record = [];
    continue;
  }

  field += ch;
}

if (field.length || record.length) {
  endField();
  processRecord(record);
}

console.log({ file: inputPath, rows, parsed, missing, past, today: todayCnt, future });
if (future > 0) {
  console.error("Found future Publish_Date values. Samples:");
  for (const s of futureSamples) console.error(" -", s);
  process.exit(1);
}

process.exit(0);

