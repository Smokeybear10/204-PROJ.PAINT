document.documentElement.classList.replace('no-js', 'js');

(function () {

  // ── elements ──────────────────────────────────────────────────
  var pb          = document.getElementById('pb');
  var masthead    = document.getElementById('masthead');
  var heroWrap    = document.getElementById('heroWrap');
  var canvasStage = document.getElementById('canvasStage');
  var showcase    = document.getElementById('canvasShowcase');
  var colMarker   = document.getElementById('colMarker');
  var scrollCue   = document.getElementById('scrollCue');
  var brush1      = document.querySelector('.hero-brush-1');
  var brush2      = document.querySelector('.hero-brush-2');
  var brush3      = document.querySelector('.hero-brush-3');
  var demosWrap   = document.getElementById('demosWrap');
  var demoTrack   = document.getElementById('demoTrack');
  var demoCards   = document.querySelectorAll('#demoTrack .demo-card');
  var buildWrap   = document.getElementById('buildWrap');
  var archEl      = document.querySelector('.arch');
  var footerEl    = document.querySelector('footer');
  var archLabel   = document.getElementById('archLabel');
  var archH2      = document.getElementById('archH2');
  var archRule    = document.getElementById('archRule');
  var archItems   = document.querySelectorAll('.arch-item');

  // ── helpers ───────────────────────────────────────────────────
  function clamp(v, lo, hi) { return Math.min(Math.max(v, lo), hi); }
  // smooth S-curve: slow in, fast middle, slow out
  function smoothstep(lo, hi, t) {
    t = clamp((t - lo) / (hi - lo), 0, 1);
    return t * t * (3 - 2 * t);
  }

  // ── hero scroll: drives all .hs-el reveals ────────────────────
  function heroProgress() {
    var rect  = heroWrap.getBoundingClientRect();
    var total = heroWrap.offsetHeight - window.innerHeight;
    return total > 0 ? clamp(-rect.top / total, 0, 1) : 0;
  }

  function updateHero() {
    var p = heroProgress();
    // canvas tilt: starts slightly tilted, flattens as user scrolls
    var ct = smoothstep(0.0, 0.8, p);
    var rx = 20 * (1 - ct);
    var sc = 1.06 - 0.06 * ct;
    // parallax: canvas drifts up as user scrolls out of stage
    var ty = p * -55;
    if (showcase) {
      showcase.style.transform =
        'rotate(-0.5deg) rotateX(' + rx.toFixed(2) + 'deg) scale(' + sc.toFixed(4) + ')';
    }
    if (canvasStage) {
      canvasStage.style.transform = 'translateY(' + ty.toFixed(1) + 'px)';
    }
  }

  // ── demos carousel scroll stage ───────────────────────────────
  function demoProgress() {
    if (!demosWrap || window.innerWidth <= 780) return 0;
    var rect  = demosWrap.getBoundingClientRect();
    var total = demosWrap.offsetHeight - window.innerHeight;
    return total > 0 ? clamp(-rect.top / total, 0, 1) : 0;
  }

  // Unclamped version so entrance can start before section hits viewport top
  function demoProgressRaw() {
    if (!demosWrap || window.innerWidth <= 780) return 0;
    var rect  = demosWrap.getBoundingClientRect();
    var total = demosWrap.offsetHeight - window.innerHeight;
    return total > 0 ? -rect.top / total : 0;
  }

  function updateDemos() {
    if (!demoTrack || window.innerWidth <= 780) return;
    var pRaw = demoProgressRaw();
    var p    = clamp(pRaw, 0, 1);

    // Piecewise: dwell at card 0, sweep 0→1, dwell, sweep 1→2, dwell
    var t;
    if      (p < 0.12) { t = 0; }
    else if (p < 0.40) { t = smoothstep(0.12, 0.40, p); }
    else if (p < 0.55) { t = 1; }
    else if (p < 0.83) { t = 1 + smoothstep(0.55, 0.83, p); }
    else               { t = 2; }

    // Center the active card within the carousel outer
    var outer     = demoTrack.parentElement;
    var outerW    = outer.offsetWidth;
    var firstCard = demoTrack.firstElementChild;
    if (!firstCard) return;
    var cardW  = firstCard.offsetWidth;
    var cards  = demoTrack.children;
    var gap    = cards.length > 1 ? cards[1].getBoundingClientRect().left - cards[0].getBoundingClientRect().right : 40;
    var step   = cardW + gap;
    var startX = (outerW - cardW) / 2;
    var tx     = startX - t * step;

    demoTrack.style.transform = 'translateX(' + tx.toFixed(1) + 'px)';

    demoCards.forEach(function (card, i) {
      // Entrance starts earlier (pRaw ≈ -0.35) but finishes at same point
      var lo    = -0.35 + i * 0.03;
      var hi    =  0.09 + i * 0.025;
      var enter = smoothstep(lo, hi, pRaw);

      var dist  = Math.abs(i - t);
      var sc    = Math.max(0.92, 1 - dist * 0.04);

      if (enter < 0.999) {
        card.style.transition = 'none';
        var ex = ((1 - enter) * 110).toFixed(1);
        card.style.transform = 'translateX(' + ex + 'vw) scale(var(--js-sc, 1)) rotate(var(--r))';
        card.style.opacity   = '1';
      } else {
        if (card.style.transition === 'none') {
          card.style.transition = '';
          card.style.transform  = '';
        }
        // Dimmed when off-center, full opacity when centered
        var alpha = Math.max(0.45, 1 - dist * 0.55);
        card.style.opacity = alpha.toFixed(3);
      }
      card.style.setProperty('--js-sc', sc.toFixed(4));
    });

    // Arch sweeps in from right during the final demo card sweep (card 2 → tile maker)
    if (archEl && window.innerWidth > 780) {
      var archEnter = smoothstep(0.83, 1.0, p);
      archEl.style.transform    = 'translateX(' + ((1 - archEnter) * 105).toFixed(1) + 'vw)';
      archEl.style.pointerEvents = archEnter >= 0.99 ? 'auto' : 'none';

      // Drive label/h2 during arch sweep so content appears immediately with no gap
      if (archLabel) {
        var lT = smoothstep(0.88, 0.98, p);
        archLabel.style.opacity   = lT.toFixed(4);
        archLabel.style.transform = 'translateY(' + ((1 - lT) * 115).toFixed(1) + '%)';
      }
      if (archH2) {
        var hT = smoothstep(0.92, 1.0, p);
        archH2.style.opacity   = hT.toFixed(4);
        archH2.style.transform = 'translateY(' + ((1 - hT) * 115).toFixed(1) + '%)';
      }
    }
  }

  // ── arch scroll stage ─────────────────────────────────────────
  function buildProgress() {
    if (!buildWrap || window.innerWidth <= 780) return 1;
    var rect  = buildWrap.getBoundingClientRect();
    var total = buildWrap.offsetHeight - window.innerHeight;
    return total > 0 ? clamp(-rect.top / total, 0, 1) : 0;
  }

  function updateBuild() {
    var p = buildProgress();

    // Footer slides up from below at the end of this scroll stage
    if (footerEl && window.innerWidth > 780) {
      var footerT = smoothstep(0.82, 1.0, p);
      footerEl.style.transform     = 'translateY(' + ((1 - footerT) * 100).toFixed(1) + '%)';
      footerEl.style.pointerEvents = footerT >= 0.99 ? 'auto' : 'none';
    }

    // Label/H2 already driven by updateDemos — just clamp to 1 so they stay visible
    if (archLabel && parseFloat(archLabel.style.opacity) < 1) archLabel.style.opacity = '1';
    if (archH2    && parseFloat(archH2.style.opacity)    < 1) archH2.style.opacity    = '1';

    // Rule and items animate in as soon as buildWrap starts scrolling
    var ruleT = smoothstep(0.00, 0.12, p);
    if (archRule) archRule.style.transform = 'scaleX(' + ruleT.toFixed(4) + ')';

    var beats = [
      smoothstep(0.12, 0.25, p),
      smoothstep(0.27, 0.40, p),
      smoothstep(0.42, 0.55, p),
      smoothstep(0.57, 0.70, p)
    ];
    archItems.forEach(function (item, i) {
      var t  = beats[i];
      var dx = (i % 2 === 0 ? -1 : 1) * (1 - t) * 64;
      item.style.opacity   = t.toFixed(4);
      item.style.transform = 'translateX(' + dx.toFixed(1) + 'px)';
      item.style.setProperty('--bh', (t * 100).toFixed(1) + '%');
    });
  }

  // ── global scroll: progress bar, masthead, col marker ─────────
  function onScroll() {
    var sy  = window.scrollY;
    var doc = document.documentElement;
    var total = doc.scrollHeight - doc.clientHeight;

    pb.style.width = (total > 0 ? sy / total * 100 : 0).toFixed(1) + '%';

    masthead.classList.toggle('scrolled', sy > 10);
    colMarker.classList.toggle('visible', sy > 300);
    if (scrollCue) scrollCue.classList.toggle('hidden', sy > 40);

    updateHero();
    updateDemos();
    updateBuild();
  }

  // ── mouse parallax on hero brushstrokes ───────────────────────
  var tmx = 0, tmy = 0, cmx = 0, cmy = 0;
  var heroEl = document.getElementById('home');

  heroEl.addEventListener('mousemove', function (e) {
    var r = heroEl.getBoundingClientRect();
    tmx = (e.clientX - r.left) / r.width  - 0.5;
    tmy = (e.clientY - r.top)  / r.height - 0.5;
  });
  heroEl.addEventListener('mouseleave', function () { tmx = 0; tmy = 0; });

  function parallaxTick() {
    cmx += (tmx - cmx) * 0.06;
    cmy += (tmy - cmy) * 0.06;
    if (brush1) brush1.style.transform = 'rotate(-11deg) translate(' + (cmx * -22).toFixed(1) + 'px,' + (cmy * -14).toFixed(1) + 'px)';
    if (brush2) brush2.style.transform = 'rotate(7deg)   translate(' + (cmx *  16).toFixed(1) + 'px,' + (cmy *  10).toFixed(1) + 'px)';
    if (brush3) brush3.style.transform = 'rotate(18deg)  translate(' + (cmx * -10).toFixed(1) + 'px,' + (cmy *   8).toFixed(1) + 'px)';
    requestAnimationFrame(parallaxTick);
  }


  // ── IntersectionObserver for below-hero reveals ───────────────
  if ('IntersectionObserver' in window) {
    var io = new IntersectionObserver(function (entries) {
      entries.forEach(function (e) {
        if (e.isIntersecting) { e.target.classList.add('revealed'); io.unobserve(e.target); }
      });
    }, { threshold: 0.12 });
    document.querySelectorAll('[data-reveal]').forEach(function (el) { io.observe(el); });
  } else {
    document.querySelectorAll('[data-reveal]').forEach(function (el) { el.classList.add('revealed'); });
  }

  // ── init ──────────────────────────────────────────────────────
  window.addEventListener('scroll', onScroll, { passive: true });
  window.addEventListener('resize', onScroll);
  onScroll();
  parallaxTick();

})();
