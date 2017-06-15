% First paragraph.
?- sentence(110,X), p(Z,X,[]).
X = [insulin, isa, peptide, hormone, that, is, produced, by, betacell, of, pancreatic, islet],
Z = [p(np(n(insulin), mod([]), ext([])), vp(verb(active(isa), mod([])), np(n(hormone),
    mod([cn([characterization], n(peptide)), rc(verb(passive(produced), mod([])),
    np(n(betacell), mod([pp(of, [part_of], np(n(islet), mod([adj(pancreatic)]),
    ext([])))]), ext([])))]), ext([]))))] .

?- sentence(120,X), p(Z,X,[]).
X = [insulin, regulate, by, promotion, of, absorption, of, glucose, from, blood, into,
    fat, into, liver, and, into, skeletal, muscle, metabolism, of, carbohydrate],
Z = [p(np(n(insulin), mod([]), ext([])), vp(verb(active(regulate), mod([pp(by, [manner],
    np(n(promotion), mod([pp(of, [patient], np(n(absorption), mod([pp(of, [patient],
    np(n(glucose), mod([]), ext([]))), pp(from, [source], np(n(blood), mod([]), ext([]))),
    pp(into, [direction], np(n(fat), mod([]), ext([]))), pp(into, [direction],
    np(n(liver), mod([]), ext([]))), pp(into, [direction], np(n(muscle),
    mod([adj(skeletal)]), ext([])))]), ext([])))]), ext([])))])), np(n(metabolism),
    mod([pp(of, [patient], np(n(carbohydrate), mod([]), ext([])))]), ext([]))))] 

?- sentence(121,X), p(Z,X,[]).
X = [insulin, regulate, by, promotion, of, absorption, of, glucose, from, blood, into,
    fat, into, liver, and, into, skeletal, muscle, metabolism, of, fat],
Z = [p(np(n(insulin), mod([]), ext([])), vp(verb(active(regulate), mod([pp(by, [manner],
    np(n(promotion), mod([pp(of, [patient], np(n(absorption), mod([pp(of, [patient],
    np(n(glucose), mod([]), ext([]))), pp(from, [source], np(n(blood), mod([]), ext([]))),
    pp(into, [direction], np(n(fat), mod([]), ext([]))), pp(into, [direction],
    np(n(liver), mod([]), ext([]))), pp(into, [direction], np(n(muscle),
    mod([adj(skeletal)]), ext([])))]), ext([])))]), ext([])))])), np(n(metabolism),
    mod([pp(of, [patient], np(n(fat), mod([]), ext([])))]), ext([]))))] 

?- sentence(122,X), p(Z,X,[]).
X = [insulin, regulate, by, promotion, of, absorption, of, glucose, from, blood, into,
    fat, into, liver, and, into, skeletal, muscle, metabolism, of, protein],
Z = [p(np(n(insulin), mod([]), ext([])), vp(verb(active(regulate), mod([pp(by, [manner],
    np(n(promotion), mod([pp(of, [patient], np(n(absorption), mod([pp(of, [patient],
    np(n(glucose), mod([]), ext([]))), pp(from, [source], np(n(blood), mod([]), ext([]))),
    pp(into, [direction], np(n(fat), mod([]), ext([]))), pp(into, [direction],
    np(n(liver), mod([]), ext([]))), pp(into, [direction], np(n(muscle),
    mod([adj(skeletal)]), ext([])))]), ext([])))]), ext([])))])), np(n(metabolism),
    mod([pp(of, [patient], np(n(protein), mod([]), ext([])))]), ext([]))))] 

?- sentence(130,X), p(Z,X,[]).
X = [absorbed, glucose, is, converted, in, fat, ",", in, liver, and, in, skeletal, muscle, 
    cell, into, glycogen, by, glycogenesis],
Z = [p(np(n(glucose), mod([adj(absorbed)]), ext([])), vp(verb(passive(converted), mod([pp(in,
    [location], np(n(fat), mod([]), ext([]))), pp(in, [location], np(n(liver), mod([]),
    ext([]))), pp(in, [location], np(n(cell), mod([adj(skeletal), cn([characterization],
    n(muscle))]), ext([]))), pp(into, [result], np(n(glycogen), mod([]), ext([]))), pp(by,
    [manner], np(n(glycogenesis), mod([]), ext([])))]))))] .

?- sentence(131,X), p(Z,X,[]).
X = [absorbed, glucose, is, converted, in, fat, ",", in, liver, and, in, skeletal, muscle, 
    cell, into, fat, ",", which, isa, triglyceride, ",", by, lipogenesis],
Z = [p(np(n(glucose), mod([adj(absorbed)]), ext([])), vp(verb(passive(converted), mod([pp(in,
    [location], np(n(fat), mod([]), ext([]))), pp(in, [location], np(n(liver), mod([]),
    ext([]))), pp(in, [location], np(n(cell), mod([adj(skeletal), cn([characterization],
    n(muscle))]), ext([]))), pp(into, [result], np(n(fat), mod([]), ext([pc(verb(active(isa),
    mod([])), np(n(triglyceride), mod([]), ext([])))]))), pp(by, [manner], np(n(lipogenesis),
    mod([]), ext([])))]))))] 

?- sentence(132,X), p(Z,X,[]).
X = [absorbed, glucose, is, converted, in, liver, into, glycogen, and, into, fat],
Z = [p(np(n(glucose), mod([adj(absorbed)]), ext([])), vp(verb(passive(converted), mod([pp(in,
    [location], np(n(liver), mod([]), ext([]))), pp(into, [result], np(n(glycogen), mod([]),
    ext([]))), pp(into, [result], np(n(fat), mod([]), ext([])))]))))] .

?- sentence(140,X), p(Z,X,[]).
X = [glucose, production, by, liver, is, inhibited, by, high, insulin, concentration, in , blood],
Z = [p(np(n(production), mod([cn([result], n(glucose)), pp(by, [agent], np(n(liver),
    mod([]), ext([])))]), ext([])), vp(verb(passive(inhibited), mod([])),
    np(n(concentration), mod([adj(high), cn([bearer], n(insulin)), pp(in, [location],
    np(n(blood), mod([]), ext([])))]), ext([]))))] .

?- sentence(141,X), p(Z,X,[]).
X = [glucose, excretion, into, blood, by, liver, is, inhibited, by, high, insulin, 
    concentration, in, blood],
Z = [p(np(n(excretion), mod([cn([result], n(glucose)), pp(into, [direction], np(n(blood),
    mod([]), ext([]))), pp(by, [agent], np(n(liver), mod([]), ext([])))]), ext([])),
    vp(verb(passive(inhibited), mod([])), np(n(concentration), mod([adj(high),
    cn([bearer], n(insulin)), pp(in, [location], np(n(blood), mod([]), ext([])))]),
    ext([]))))] .

?- sentence(150,X), p(Z,X,[]).
X = [circulating, insulin, affect, synthesis, in, tissue, of, protein],
Z = [p(np(n(insulin), mod([adj(circulating)]), ext([])), vp(verb(active(affect), mod([])),
    np(n(synthesis), mod([pp(in, [location], np(n(tissue), mod([pp(of, [part_of],
    np(n(protein), mod([]), ext([])))]), ext([])))]), ext([]))))] .

?- sentence(160,X), p(Z,X,[]).
X = [insulin, isa, at, high, insulin, concentration, in, blood, anabolic, hormone, that,
    promote, conversion, of, small, molecule, in, blood, into, large, molecule, in, cell],
Z = [p(np(n(insulin), mod([]), ext([])), vp(verb(active(isa), mod([pp(at, [condition], 
    np(n(concentration), mod([adj(high), cn([bearer], n(insulin)), pp(in, [location],
    np(n(blood), mod([]), ext([])))]), ext([])))])), np(n(hormone), mod([adj(anabolic),
    rc(verb(active(promote), mod([])), np(n(conversion), mod([pp(of, [patient],
    np(n(molecule), mod([adj(small)]), ext([]))), pp(in, [location], np(n(blood), mod([]),
    ext([]))), pp(into, [result], np(n(molecule), mod([adj(large), pp(in, [location],
    np(n(cell), mod([]), ext([])))]), ext([])))]), ext([])))]), ext([]))))] .

?- sentence(170,X), p(Z,X,[]).
X = [insulin, promote, at, low, insulin, concentration, widespread, catabolism],
Z = [p(np(n(insulin), mod([]), ext([])), vp(verb(active(promote), mod([pp(at, [condition],
    np(n(concentration), mod([adj(low), cn([bearer], n(insulin))]), ext([])))])),
    np(n(catabolism), mod([adj(widespread)]), ext([]))))] .


% Second paragraph.
?- sentence(210,X), p(Z,X,[]).
X = [pancreatic, betacell, is, affected, by, glucose, concentration, in, blood],
Z = [p(np(n(betacell), mod([adj(pancreatic)]), ext([])), vp(verb(passive(affected), mod([])),
    np(n(concentration), mod([cn([bearer], n(glucose)), pp(in, [location], np(n(blood), mod([]),
    ext([])))]), ext([]))))] 

?- sentence(220,X), p(Z,X,[]).
X = [betacell, secrete, at, high, glucose, concentration, insulin],
Z = [p(np(n(betacell), mod([]), ext([])), vp(verb(active(secrete), mod([pp(at, [condition],
    np(n(concentration), mod([adj(high), cn([bearer], n(glucose))]), ext([])))])), np(n(insulin),
    mod([]), ext([]))))] .

?- sentence(221,X), p(Z,X,[]).
X = [betacell, stop, at, low, glucose, concentration, insulin, secretion, into, general, circulation],
Z = [p(np(n(betacell), mod([]), ext([])), vp(verb(active(stop), mod([pp(at, [condition], 
    np(n(concentration), mod([adj(low), cn([bearer], n(glucose))]), ext([])))])), np(n(secretion),
    mod([cn([result], n(insulin)), pp(into, [direction], np(n(circulation), mod([adj(general)]),
    ext([])))]), ext([]))))] .

?- sentence(230,X), p(Z,X,[]).
X = [alphacell, take, from, betacell, cue],
Z = [p(np(n(alphacell), mod([]), ext([])), vp(verb(active(take), mod([pp(from, [source],
    np(n(betacell), mod([]), ext([])))])), np(n(cue), mod([]), ext([]))))] .

?- sentence(231,X), p(Z,X,[]).
X = [alphacell, secrete, into, blood, and, in, opposite, manner, as, betacell, glucagon],
Z = [p(np(n(alphacell), mod([]), ext([])), vp(verb(active(secrete), mod([pp(into, [direction],
    np(n(blood), mod([]), ext([]))), pp(in, [manner], np(n(manner), mod([adj(opposite), pp(as,
    [comparison], np(n(betacell), mod([]), ext([])))]), ext([])))])), np(n(glucagon), mod([]),
    ext([]))))] .

?- sentence(232,X), p(Z,X,[]).
X = [alphacell, secrete, highly, at, high, glucose, concentration, glucagon],
Z = [p(np(n(alphacell), mod([]), ext([])), vp(verb(active(secrete), mod([adv(highly), pp(at,
    [condition], np(n(concentration), mod([adj(high), cn([bearer], n(glucose))]), ext([])))])),
    np(n(glucagon), mod([]), ext([]))))] .

?- sentence(233,X), p(Z,X,[]).
X = [alphacell, secrete, lowly, at, low, glucose, concentration, glucagon],
Z = [p(np(n(alphacell), mod([]), ext([])), vp(verb(active(secrete), mod([adv(lowly), pp(at,
    [condition], np(n(concentration), mod([adj(low), cn([bearer], n(glucose))]), ext([])))])),
    np(n(glucagon), mod([]), ext([]))))]

?- sentence(240,X), p(Z,X,[]).
X = [high, glucagon, concentration, in, blood, plasma, stimulate, powerfully, liver, release, of,
    glucagon, in, blood, by, glycogenolysis, and, by, gluconeogenesis],
Z = [p(np(n(concentration), mod([adj(high), cn([bearer], n(glucagon)), pp(in, [location],
    np(n(plasma), mod([cn([characterization], n(blood))]), ext([])))]), ext([])),
    vp(verb(active(stimulate), mod([adv(powerfully)])), np(n(release), mod([cn([agent], n(liver)),
    pp(of, [patient], np(n(glucagon), mod([]), ext([]))), pp(in, [location], np(n(blood), mod([]),
    ext([]))), pp(by, [manner], np(n(glycogenolysis), mod([]), ext([]))), pp(by, [manner],
    np(n(gluconeogenesis), mod([]), ext([])))]), ext([]))))] 

?- sentence(241,X), p(Z,X,[]).
X = [stimulation, of, liver, glucose, release, in, blood, by, glycogenolysis, and, by, 
    gluconeogenesis, affect, in, opposite, manner, as, effect, that, is, produced, by, high,
    insulin, concentration, blood, glucose, concentration],
Z = [p(np(n(stimulation), mod([pp(of, [patient], np(n(release), mod([cn([agent], n(liver)),
    cn([result], n(glucose)), pp(in, [location], np(n(blood), mod([]), ext([]))), pp(by, [manner],
    np(n(glycogenolysis), mod([]), ext([]))), pp(by, [manner], np(n(gluconeogenesis), mod([]),
    ext([])))]), ext([])))]), ext([])), vp(verb(active(affect), mod([pp(in, [manner], np(n(manner),
    mod([adj(opposite), pp(as, [comparison], np(n(effect), mod([rc(verb(passive(produced),
    mod([])), np(n(concentration), mod([adj(high), cn([bearer], n(insulin))]), ext([])))]),
    ext([])))]), ext([])))])), np(n(concentration), mod([cn([bearer], n(blood)), cn([bearer],
    n(glucose))]), ext([]))))] .

?- sentence(250,X), p(Z,X,[]).
X = [secretion, of, insulin, into, blood, in_response_to, blood, glucose, concentration, isa,
    primary, mechanism, that, keep, within, narrow, limit, glucose, concentration, in,
    extracellular, fluid],
Z = [p(np(n(secretion), mod([pp(of, [result], np(n(insulin), mod([]), ext([]))), pp(into,
    [direction], np(n(blood), mod([]), ext([]))), pp(in_response_to, [cause], np(n(concentration),
    mod([cn([bearer], n(blood)), cn([bearer], n(glucose))]), ext([])))]), ext([])),
    vp(verb(active(isa), mod([])), np(n(mechanism), mod([adj(primary), rc(verb(active(keep),
    mod([pp(within, [location], np(n(limit), mod([adj(narrow)]), ext([])))])), np(n(concentration),
    mod([cn([bearer], n(glucose)), pp(in, [location], np(n(fluid), mod([adj(extracellular)]),
    ext([])))]), ext([])))]), ext([]))))] .
