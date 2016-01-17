cnf(con_p1, axiom, in(X1, con(X1, Z))).
cnf(con_p2, axiom, in(X1, con(Y1, Z)) | ~in(X1, Z)).

cnf(par0, axiom, ~ pi0(not(P), X1) | ~ pi0(P, X1)).
cnf(par1, axiom, ~ pi0(or(P, Q), X1) | pi0(P, X1) | pi0(Q, X1)).
cnf(par3, axiom, ~ pi0(and(P, Q), X1) | pi0(Q, X1)).
cnf(par2, axiom, ~ pi0(and(P, Q), X1) | pi0(P, X1)).

cnf(ctl_ax, axiom, ~ pi0(ax(P), X1) | ~r(X1, con(Z1, Z)) | pi1(land(P), con(Z1, Z))).

cnf(ctl_ex, axiom, ~ pi0(ex(P), X1) | ~r(X1, con(Z1, Z)) | pi1(lor(P), con(Z1, Z))).

cnf(ctl_eg1, axiom, ~ pi0(eg(P), X1) | ~r(X1, con(Z1, Z)) | pi1(lor(eg(P)), con(Z1, Z))).
cnf(ctl_eg2, axiom, ~ pi0(eg(P), X1) | pi0(P, X1)).

cnf(ctl_ag1, axiom, ~ pi0(ag(P), X1) | ~r(X1, con(Z1, Z)) | pi1(land(ag(P)), con(Z1, Z))).
cnf(ctl_ag2, axiom, ~ pi0(ag(P), X1) | pi0(P, X1)).

cnf(ctl_af1, axiom, ~ pi0(af(P), X1) | pi2(af(P), X1, nil)).
cnf(ctl_af2, axiom, ~ pi2(af(P), X1, Y)| pi0(P, X1) | ~r(X1, con(Z1, Z)) | pi3(land(af(P)), con(Z1, Z), con(X1, Y))).
cnf(ctl_af3, axiom, ~ pi2(af(P), X1, Y) | ~in(X1,Y)).

cnf(ctl_ef1, axiom, ~ pi0(ef(P), X1) | pi2(ef(P), X1, nil)).
cnf(ctl_ef2, axiom, ~ pi2(ef(P), X1, Y) |pi0(P, X1) | ~r(X1, con(Z1, Z)) | pi3(lor(ef(P)), con(Z1, Z), con(X1, Y))).
cnf(ctl_ef3, axiom, ~ pi2(ef(P), X1, Y) | ~in(X1,Y)).

cnf(ctl_au1, axiom, ~ pi0(au(P, Q), X1) | pi2(au(P, Q), X1, nil)).
cnf(ctl_au2, axiom, ~ pi2(au(P, Q), X1, Y)| pi0(Q, X1) | ~r(X1, con(Z1, Z)) | pi3(land(au(P, Q)), con(Z1, Z), con(X1, Y))).
cnf(ctl_au2, axiom, ~ pi2(au(P, Q), X1, Y)| pi0(Q, X1) | pi0(P, X1)).
cnf(ctl_au3, axiom, ~ pi2(au(P, Q), X1, Y) | ~in(X1,Y)).

cnf(ctl_eu1, axiom, ~ pi0(eu(P, Q), X1) | pi2(eu(P, Q), X1, nil)).
cnf(ctl_eu2, axiom, ~ pi2(eu(P, Q), X1, Y)| pi0(Q, X1) | ~r(X1, con(Z1, Z)) | pi3(lor(eu(P, Q)), con(Z1, Z), con(X1, Y))).
cnf(ctl_eu2, axiom, ~ pi2(eu(P, Q), X1, Y)| pi0(Q, X1) | pi0(P, X1)).
cnf(ctl_eu3, axiom, ~ pi2(eu(P, Q), X1, Y) | ~in(X1,Y)).

cnf(ctl_er1, axiom, ~ pi0(er(P, Q), X1) | pi0(P, X1) | ~r(X1, con(Z1, Z)) | pi1(lor(er(P, Q)), con(Z1, Z))).
cnf(ctl_er2, axiom, ~ pi0(er(P, Q), X1) | pi0(Q, X1)).

cnf(ctl_ar1, axiom, ~ pi0(ar(P, Q), X1) | pi0(P, X1) |~r(X1, con(Z1, Z)) | pi1(land(ar(P, Q)), con(Z1, Z))).
cnf(ctl_ar2, axiom, ~ pi0(ar(P, Q), X1) | pi0(Q, X1)).

cnf(ctl_land1, axiom, ~ pi1(land(P), con(X1, Z)) | pi1(land(P), Z)).
cnf(ctl_land2, axiom, ~ pi1(land(P), con(X1, Z)) | pi0(P, X1)).
cnf(ctl_land3, axiom, ~ pi3(land(P), con(X1,Z), Y) | pi3(land(P), Z, Y)).
cnf(ctl_land4, axiom, ~ pi3(land(P), con(X1,Z), Y) | pi2(P, X1, Y)).

cnf(ctl_lor1, axiom, ~ pi1(lor(P), con(X1, Z)) |  pi0(P, X1) | pi1(lor(P), Z)).
cnf(ctl_lor2, axiom, ~ pi1(lor(P), nil)).
cnf(ctl_lor3, axiom, ~ pi3(lor(P), con(X1, Z), Y) | pi2(P, X1, Y) | pi3(lor(P), Z, Y)).
cnf(ctl_lor4, axiom, ~ pi3(lor(P), nil, Y)).

cnf(p1f, axiom, ~pi0(p1, state(b(ff,tt), b(T2,F2), b(T3,F3), b(T4,F4), b(T5,F5), b(T6,F6), b(T7,F7), b(T8,F8), b(T9,F9), b(T10,F10), b(T11,F11), b(T12,F12)))).
cnf(p1t, axiom,  pi0(p1, state(b(tt,ff), b(T2,F2), b(T3,F3), b(T4,F4), b(T5,F5), b(T6,F6), b(T7,F7), b(T8,F8), b(T9,F9), b(T10,F10), b(T11,F11), b(T12,F12)))).
cnf(p2f, axiom, ~pi0(p2, state(b(T1,F1), b(ff,tt), b(T3,F3), b(T4,F4), b(T5,F5), b(T6,F6), b(T7,F7), b(T8,F8), b(T9,F9), b(T10,F10), b(T11,F11), b(T12,F12)))).
cnf(p2t, axiom,  pi0(p2, state(b(T1,F1), b(tt,ff), b(T3,F3), b(T4,F4), b(T5,F5), b(T6,F6), b(T7,F7), b(T8,F8), b(T9,F9), b(T10,F10), b(T11,F11), b(T12,F12)))).
cnf(p3f, axiom, ~pi0(p3, state(b(T1,F1), b(T2,F2), b(ff,tt), b(T4,F4), b(T5,F5), b(T6,F6), b(T7,F7), b(T8,F8), b(T9,F9), b(T10,F10), b(T11,F11), b(T12,F12)))).
cnf(p3t, axiom,  pi0(p3, state(b(T1,F1), b(T2,F2), b(tt,ff), b(T4,F4), b(T5,F5), b(T6,F6), b(T7,F7), b(T8,F8), b(T9,F9), b(T10,F10), b(T11,F11), b(T12,F12)))).
cnf(p4f, axiom, ~pi0(p4, state(b(T1,F1), b(T2,F2), b(T3,F3), b(ff,tt), b(T5,F5), b(T6,F6), b(T7,F7), b(T8,F8), b(T9,F9), b(T10,F10), b(T11,F11), b(T12,F12)))).
cnf(p4t, axiom,  pi0(p4, state(b(T1,F1), b(T2,F2), b(T3,F3), b(tt,ff), b(T5,F5), b(T6,F6), b(T7,F7), b(T8,F8), b(T9,F9), b(T10,F10), b(T11,F11), b(T12,F12)))).
cnf(p5f, axiom, ~pi0(p5, state(b(T1,F1), b(T2,F2), b(T3,F3), b(T4,F4), b(ff,tt), b(T6,F6), b(T7,F7), b(T8,F8), b(T9,F9), b(T10,F10), b(T11,F11), b(T12,F12)))).
cnf(p5t, axiom,  pi0(p5, state(b(T1,F1), b(T2,F2), b(T3,F3), b(T4,F4), b(tt,ff), b(T6,F6), b(T7,F7), b(T8,F8), b(T9,F9), b(T10,F10), b(T11,F11), b(T12,F12)))).
cnf(p6f, axiom, ~pi0(p6, state(b(T1,F1), b(T2,F2), b(T3,F3), b(T4,F4), b(T5,F5), b(ff,tt), b(T7,F7), b(T8,F8), b(T9,F9), b(T10,F10), b(T11,F11), b(T12,F12)))).
cnf(p6t, axiom,  pi0(p6, state(b(T1,F1), b(T2,F2), b(T3,F3), b(T4,F4), b(T5,F5), b(tt,ff), b(T7,F7), b(T8,F8), b(T9,F9), b(T10,F10), b(T11,F11), b(T12,F12)))).


cnf(re1, axiom, r(state(b(T1,F1), b(T2,F2), b(T3,F3), b(T4,F4), b(T5,F5), b(T6,F6), b(T7,F7), b(T8,F8), b(T9,F9), b(T10,F10), b(T11,F11), b(T12,F12)),
              con(state(b(F6,T6), b(F4,T4), b(F4,T4), b(F6,T6), b(F2,T2), b(F6,T6), b(F3,T3), b(F5,T5), b(T9,F9), b(T10,F10), b(T11,F11), b(T12,F12)),
              con(state(b(F4,T4), b(F10,T10), b(F9,T9), b(F10,T10), b(F3,T3), b(F3,T3), b(T7,F7), b(T8,F8), b(F5,T5), b(F2,T2), b(T11,F11), b(T12,F12)),
              con(state(b(F6,T6), b(F12,T12), b(F3,T3), b(F6,T6), b(F3,T3), b(F4,T4), b(T7,F7), b(T8,F8), b(T9,F9), b(T10,F10), b(F6,T6), b(F11,T11)),
              nil))))).

cnf(check, negated_conjecture, pi0(ag(or(p1, or(p2, or(p3, or(p4, or(p5, p6)))))), state(b(ff,tt), b(tt,ff), b(ff,tt), b(tt,ff), b(tt,ff), b(ff,tt), b(ff,tt), b(ff,tt), b(ff,tt), b(ff,tt), b(ff,tt), b(ff,tt)))).
