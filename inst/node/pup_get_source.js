// Get page source from link 
const puppeteer = require('puppeteer');
const fs = require('fs');
(async () => {
  // pass arguments from R
  const path_to_file = process.argv[2]; 
  const link = process.argv[3];
  // navigate and get source
  const browser = await puppeteer.launch();
  const page = await browser.newPage();
  await page.goto(link, {waitUntil: "networkidle0"});
  // await page.waitFor(300)
  const html = await page.content();
	fs.writeFileSync(path_to_file, html);
  await browser.close();
})();
