/* eslint-disable no-restricted-syntax, no-await-in-loop, no-console */
const fs = require("fs");
const path = require("path");
const fetch = require("node-fetch");

let names =
  "mal malc2 malc2mev malu mmal malsme malsmp angstar u tuj muc2mev uev uhr uhz uminv uj uk ukg auhypol au2hypol tthbar te aucd aucur auedm auefld auefg auepol auep aueqm thr auforce tbohrrada0 aumdm aumfd aumag ttme aumom auperm aut auvel na mub mubev mubshhz mubshcminv mubskk bohrrada0 k tkev kshhz kshcminv z0 re ecomwl ecomwlbar conqu2e2sh kj90 rk90 xucukalph1 gdn mud mudsmub mudsmun md mdc2 mdc2mev mdu mmd rd mudsmuem mdsme mudsmunn mudsmup mdsmp ep0 esme gem gammae gammaebar muem ae muemsmub muemsmun me mec2 mec2mev meu mme mesmalpha muemsmuhp muemsmupp tevj evu evhr evhz evminv evj evk evkg muemsmud mesmd mesmh muemsmumum mesmmu muemsmunn mesmn muemsmup mesmp mesmtau mesmt e esh f f90 gf alph c11strc c1l hr threv hru hrev hrhz hrminv hrj hrk hrkg ghn muh muhsmub muhsmun mh mhc2 mhc2mev mhu mmh mhsme mhsmp hzu hzev hzhr hzminv hzj hzk hzkg alphinv minvu minvev minvhr minvhz minvj minvk minvkg invconqu kjos ju jev jhr jhz jminv jk jkg ku kev khr khz kminv kj kkg kgu kgev kghr kghz kgminv kgj kgk asil n0 n0std mu0 flxquhs2e xumokalph1 r mu mm12c nah nahc mvol mvolstd mvolsil mcomwl mcomwlbar gmum mumum amu mumumsmub mumumsmun mmu mmuc2 mmuc2mev mmuu mmmu mmusme mmusmn mumumsmup mmusmp mmusmtau thbar thbarev tmec2 tmec2mev tecomwlbar tme mec mecmevsc nut tc ncomwl ncomwlbar gnn gamman gammanbar munn munsmub munsmun mn mnc2 mnc2mev mnu mmn munsmupp munsmue mnsme mnsmmu munsmup mnmmp mnmmpc2 mnmmpc2mev mnmmpu mnsmp mnsmtau bg bgspu mun munev munshcminv munskk munshhz h hev hbar hbarev hbcmevf plkl plkm plkmc2gev plktmp plkt esmp pcomwl pcomwlbar gp gammap gammapbar mup mupsmub mupsmun sigmapp mp mpc2 mpc2mev mpu mmp rp mpsme mpsmmu mupsmunn mpsmn mpsmtau qucirchs2me hsme ryd rydchz rydhcev rydhcj s0sr s0srstd c22ndrc gammahp gammahpbar muhp muhpsmub muhpsmun muhpsmup muhpsmupp gammapp gammappbar mupp muppsmub muppsmun c gn stdatm stdspr sigma tcomwl tcomwlbar mtau mtauc2 mtauc2mev mtauu mmtau mtausme mtausmmu mtausmn mtausmp sigmae gtn mut mutsmub mutsmun mt mtc2 mtc2mev mtu mmt mtsme mtsmp tukg rk sin2th bpwien bwien d220sil";
names = names.split(" ");

(async () => {
  const out = [];

  for (const name of names) {
    const url = `https://physics.nist.gov/cgi-bin/cuu/Value?${name}`;
    const res = await fetch(url);
    const text = await res.text();
    try {
      const title = text
        .match(/<title>CODATA Value:([^>]+)<\/title>/)[1]
        .trim();
      const tex = text
        .split("\n")
        .filter((x) => x.includes(`/cuu/Constants/Value/gif/${name}.gif`))
        .map((line) => line.match(/alt="([^"]+)/))
        .filter((line) => line != null)[0][1]
        .trim();
      out.push({ name, title, tex });
      console.log({ name, title, tex });
    } catch (e) {
      console.log({ url, name });
      console.log(e);
      throw new Error(`Failed on url`);
    }
  }

  fs.writeFileSync(
    path.join(__dirname, "/data.json"),
    "utf8",
    JSON.stringify(out)
  );
})().catch(console.error);
