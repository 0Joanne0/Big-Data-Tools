/*Projet: Webscraping*/
/*Objet: _BUT DE CE DOCUMENT_*/
/*Auteurs:*/
/* - Nom1, Prénom1*/
/* - Nom2, Prénom2*/
/* - Nom3, Prénom3*/
/* - Nom4, Prénom4*/
/* - Nom5, Prénom5*/

function syncViewToggle() {
  const $wrap = $(".explorer-page .view-toggle");
  const $checked = $("#exp_view input[type='radio']:checked");
  if ($wrap.length === 0 || $checked.length === 0) return;

  const v = $checked.val(); // "list" ou "map"
  $wrap.toggleClass("is-list", v === "list");
  $wrap.toggleClass("is-map", v === "map");
}


function initSearchMultiline() {
  function updateMultiline(pill) {
    if (!pill) return;
    const input = pill.querySelector(".selectize-input");
    if (!input) return;

    const overflow = input.scrollWidth > input.clientWidth + 5;
    pill.classList.toggle("is-multiline", overflow);
  }

  function hookPill(pill) {
    if (!pill) return;

    updateMultiline(pill);

    pill.addEventListener("input", () => updateMultiline(pill));
    pill.addEventListener("keyup", () => updateMultiline(pill));

    const obs = new MutationObserver(() => updateMultiline(pill));
    obs.observe(pill, { childList: true, subtree: true });
  }

  document.querySelectorAll(".search-pill").forEach(hookPill);

  window.addEventListener("resize", () => {
    document.querySelectorAll(".search-pill").forEach(updateMultiline);
  });
}

function initRoadCar() {
  setTimeout(function () {
    const svg  = document.getElementById("road-svg"); /* <svg> qui contient la route */
    const path = document.getElementById("motion-path"); /* path invisible : trajectoire de la voiture */
    const car  = document.getElementById("car-icon"); /* <img> de la voiture */
    const area = document.querySelector(".road-visual-area");

    /* Sécurité : si un élément manque, on stop */
    if (!svg || !path || !car || !area) return;

    /* Longueur totale du path (unité: "longueur SVG") */
    const pathLength = path.getTotalLength();

    const CAR_OFFSET_X = 0;
    const CAR_OFFSET_Y = 0;

    /* Variables calculées dynamiquement */
    let carHalfH = 0; /* moitié de la hauteur de l’image */
    let startL = 0; /* longueur de chemin à partir de laquelle la voiture est entièrement visible */

    /* ANGLE_DELTA : distance utilisée pour calculer l’angle (tangente) du path. */
    const ANGLE_DELTA = Math.max(0.9, pathLength * 0.002);

    /* Clamp : empêche une valeur de sortir d’un intervalle */
    const clamp = (v, a, b) => Math.max(a, Math.min(b, v));

    /* measureCar()
    /*  - Mesure la hauteur réelle de l'image (après chargement)
    /*  - On en déduit carHalfH pour savoir quand la voiture est "entièrement visible" */
    function measureCar() {
      const r = car.getBoundingClientRect();
      carHalfH = r.height / 2;
    }

    /* svgToAreaPx(x, y)
      - Convertit un point du path (coordonnées SVG) en coordonnées "pixels"
        dans le repère du conteneur .road-visual-area */
    function svgToAreaPx(x, y) {
      const ctm = svg.getScreenCTM(); /* matrice de transformation SVG >> écran */
      if (!ctm) return { x: 0, y: 0 };

      const pt = svg.createSVGPoint();
      pt.x = x; pt.y = y;

      /* Coordonnées du point dans le repère écran (pixels) */
      const screen = pt.matrixTransform(ctm);
      /* Coordonnées du conteneur sur l’écran */
      const areaRect = area.getBoundingClientRect();

      /* On convertit en "pixels relatifs au conteneur" */
      return {
        x: (screen.x - areaRect.left) + CAR_OFFSET_X,
        y: (screen.y - areaRect.top)  + CAR_OFFSET_Y
      };
    }

    /* computeStartL() */
    function computeStartL() {
      const p0 = path.getPointAtLength(0);
      const pos0 = svgToAreaPx(p0.x, p0.y);
      if (pos0.y >= carHalfH) {
        startL = 0;
        return;
      }

      let lo = 0, hi = pathLength;
      for (let i = 0; i < 24; i++) {
        const mid = (lo + hi) / 2;
        const p = path.getPointAtLength(mid);
        const pos = svgToAreaPx(p.x, p.y);
        if (pos.y >= carHalfH) hi = mid;
        else lo = mid;
      }
      startL = hi;
    }

    /* setCarAt(progress) */
    function setCarAt(progress) {
      progress = clamp(progress, 0, 1);

      const L = startL + progress * (pathLength - startL);
      const p  = path.getPointAtLength(L);

      const L0 = Math.max(0, L - ANGLE_DELTA);
      const L1 = Math.min(pathLength, L + ANGLE_DELTA);

      const p0 = path.getPointAtLength(L0);
      const p1 = path.getPointAtLength(L1);

      const a  = svgToAreaPx(p.x,  p.y);
      const a0 = svgToAreaPx(p0.x, p0.y);
      const a1 = svgToAreaPx(p1.x, p1.y);

      const angle = Math.atan2(a1.y - a0.y, a1.x - a0.x) * 180 / Math.PI;

      car.style.left = `${a.x}px`;
      car.style.top  = `${a.y}px`;
      car.style.transform = `translate(-50%, -50%) rotate(${angle + 90}deg)`;
    }

    /* computeProgress() */
    function computeProgress() {
      const scrollY = window.scrollY || document.documentElement.scrollTop;
      const winH = window.innerHeight;

      const r = area.getBoundingClientRect();
      const areaTop = r.top + scrollY;
      const areaH = r.height;

      const start = areaTop - winH * 0.50;
      const end = areaTop + areaH - winH * 0.25;

      return (scrollY - start) / (end - start);
    }

    let ticking = false;

    function onScroll() {
      if (ticking) return;
      ticking = true;
      requestAnimationFrame(() => {
        setCarAt(computeProgress());
        ticking = false;
      });
    }

    function initAfterLayout() {
      measureCar();
      computeStartL();
      setCarAt(0);
      onScroll();
    }

    $(window).off("scroll.carMotion resize.carMotion");
    $(window).on("scroll.carMotion", onScroll);
    $(window).on("resize.carMotion", () => {
      initAfterLayout();
    });

    if (car.complete) initAfterLayout();
    else car.addEventListener("load", initAfterLayout);

  }, 200);
}

$(document).on("shiny:connected", function () {
  initRoadCar();

  syncViewToggle();
  initSearchMultiline();
});

$(document).on("change", "#exp_view input[type='radio']", function () {
  syncViewToggle();
});

Shiny.addCustomMessageHandler("toggleIonRange", function(msg){
  function apply(){
    var $el = $("#" + msg.id);
    if ($el.length === 0) return false;

    var inst = $el.data("ionRangeSlider");
    if (inst) {
      inst.update({ disable: !!msg.disabled });
    } else {
      return false; 
    }

    var $wrap = $el.closest(".salary-slider-wrap");
    $wrap.toggleClass("is-disabled", !!msg.disabled);
    return true;
  }
  if (!apply()) setTimeout(apply, 0);
});

$(document).on("shiny:connected", function () {
  function bindToggle(inputName, toggleId){
    function update(){
      var v = $("input[name='" + inputName + "']:checked").val(); // "list" ou "map"
      var $t = $("#" + toggleId);
      if ($t.length === 0) return;

      $t.removeClass("is-list is-map")
        .addClass(v === "map" ? "is-map" : "is-list");
    }
    $(document).on("change", "input[name='" + inputName + "']", update);
    setTimeout(update, 0);
  }
  bindToggle("mp_view", "mp_view_toggle");
});

Shiny.addCustomMessageHandler("setText", function(msg){
  var $el = $("#" + msg.id);
  if ($el.length) $el.text(msg.text);
});

// Upload CV : détecte fichier + bouton X pour vider
$(document).on("change", "#mp_cv input[type='file'], #mp_cv[type='file']", function(){
  const has = this.files && this.files.length > 0;
  $(".profile-uploader").toggleClass("has-file", has);
  if (has) $(".profile-uploader").removeClass("is-error");
});

$(document).on("click", ".profile-uploader .mp-cv-remove", function(e){
  e.preventDefault();
  e.stopPropagation();

  const $f = $("#mp_cv input[type='file'], #mp_cv[type='file']");
  $f.val("");
  $f.trigger("change");
});


// Remonter en haut quand on change d'onglet
$(document).on("shiny:inputchanged", function (e) {
  if (e && e.name === "nav") {
    window.scrollTo({ top: 0, left: 0, behavior: "auto" });
  }
});


// Tourbillon de chargement 
Shiny.addCustomMessageHandler("mpLoading", function(show){
  var $sp = $("#mp_run_spinner");
  var $btn = $("#mp_run");

  if (!!show) {
    $sp.addClass("is-on");
    $btn.prop("disabled", true).addClass("is-loading");
  } else {
    $sp.removeClass("is-on");
    $btn.prop("disabled", false).removeClass("is-loading");
  }
});


Shiny.addCustomMessageHandler("mpCvError", function(msg){
  var on = !!(msg && msg.on);
  
  if (on) {
    var fe = document.querySelector("#mp_cv input[type='file']") || document.querySelector("#mp_cv[type='file']");
    var hasFile = fe && fe.files && fe.files.length > 0;
    if (hasFile) return;
  }
  
  var $u = $(".profile-uploader");
  if ($u.length === 0) return;

  if (on) {
    // reset pour rejouer l'animation shake
    $u.removeClass("is-error");
    void $u[0].offsetWidth;
    $u.addClass("is-error");
  } else {
    $u.removeClass("is-error");
  }
});

// Empêche le run si aucun CV n'est sélectionné
$(document).off("click.mp_run", "#mp_run");
$(document).on("click.mp_run", "#mp_run", function(e){

  // Toujours enlever l'erreur au clic 
  $(".profile-uploader").removeClass("is-error");

  // Récupère le vrai <input type="file"> (robuste)
  var fe = document.querySelector("#mp_cv input[type='file']") || document.querySelector("#mp_cv[type='file']");
  var has = fe && fe.files && fe.files.length > 0;

  if(!has){
    e.preventDefault();
    e.stopImmediatePropagation();

    // Ne jamais lancer le spinner si pas de CV
    $("#mp_run_spinner").removeClass("is-on");
    $("#mp_run").prop("disabled", false).removeClass("is-loading");

    // feedback rouge + rejoue l'animation
    $(".profile-uploader").removeClass("has-file is-error");
    void $(".profile-uploader")[0]?.offsetWidth;
    $(".profile-uploader").addClass("is-error");

    return false;
  }

  // OK -> spinner immédiat
  $("#mp_run_spinner").addClass("is-on");
  $("#mp_run").prop("disabled", true).addClass("is-loading");
});



