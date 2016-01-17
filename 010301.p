cnf(empty_def, axiom, ~ in(X1, nil)).
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

cnf(neq1, axiom, neq(b(ff,tt), b(tt,ff))).
cnf(neq2, axiom, neq(b(tt,ff), b(ff,tt))).


cnf(p1f, axiom, ~pi0(p1, s(C1, C2, b(ff,tt), B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12))).
cnf(p1t, axiom,  pi0(p1, s(C1, C2, b(tt,ff), B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12))).
cnf(p2f, axiom, ~pi0(p2, s(C1, C2, B1, b(ff,tt), B3, B4, B5, B6, B7, B8, B9, B10, B11, B12))).
cnf(p2t, axiom,  pi0(p2, s(C1, C2, B1, b(tt,ff), B3, B4, B5, B6, B7, B8, B9, B10, B11, B12))).
cnf(p3f, axiom, ~pi0(p3, s(C1, C2, B1, B2, b(ff,tt), B4, B5, B6, B7, B8, B9, B10, B11, B12))).
cnf(p3t, axiom,  pi0(p3, s(C1, C2, B1, B2, b(tt,ff), B4, B5, B6, B7, B8, B9, B10, B11, B12))).
cnf(p4f, axiom, ~pi0(p4, s(C1, C2, B1, B2, B3, b(ff,tt), B5, B6, B7, B8, B9, B10, B11, B12))).
cnf(p4t, axiom,  pi0(p4, s(C1, C2, B1, B2, B3, b(tt,ff), B5, B6, B7, B8, B9, B10, B11, B12))).
cnf(p5f, axiom, ~pi0(p5, s(C1, C2, B1, B2, B3, B4, b(ff,tt), B6, B7, B8, B9, B10, B11, B12))).
cnf(p5t, axiom,  pi0(p5, s(C1, C2, B1, B2, B3, B4, b(tt,ff), B6, B7, B8, B9, B10, B11, B12))).
cnf(p6f, axiom, ~pi0(p6, s(C1, C2, B1, B2, B3, B4, B5, b(ff,tt), B7, B8, B9, B10, B11, B12))).
cnf(p6t, axiom,  pi0(p6, s(C1, C2, B1, B2, B3, B4, B5, b(tt,ff), B7, B8, B9, B10, B11, B12))).

cnf(r, axiom, r(s(C1, C2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12), con(S1, con(S2, nil)))
              | ~st_eq(s(next(C1), C2, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12), S1)
              | ~st_eq(s(C1, next(C2), B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12), S2)).

cnf(next_st, axiom, st_eq(s(next(c0), C2, b(T1,F1), b(T2,F2), b(T3,F3), b(T4,F4), b(T5,F5), b(T6,F6), b(T7,F7), b(T8,F8), b(T9,F9), b(T10,F10), b(T11,F11), b(T12,F12)),
                          s(c1, C2, b(F9,T9),b(F4,T4),b(T3,F3),b(T4,F4), b(F8,T8),b(T6,F6),b(F6,T6),b(T8,F8),b(T9,F9),b(T10,F10),b(T11,F11),b(T12,F12)))).


cnf(next_st, axiom, st_eq(s(next(c1), C2, b(T1,F1), b(T2,F2), b(T3,F3), b(T4,F4), b(T5,F5), b(T6,F6), b(T7,F7), b(T8,F8), b(T9,F9), b(T10,F10), b(T11,F11), b(T12,F12)),
                          s(c2, C2, b(T1,F1),b(T2,F2),b(T3,F3),b(F8,T8),b(F2,T2),b(F1,T1),b(T7,F7),b(T8,F8),b(F7,T7),b(T10,F10),b(T11,F11),b(T12,F12)))).

cnf(next_st, axiom, st_eq(s(next(c2), C2, b(T1,F1), b(T2,F2), b(T3,F3), b(T4,F4), b(T5,F5), b(T6,F6), b(T7,F7), b(T8,F8), b(T9,F9), b(T10,F10), b(T11,F11), b(T12,F12)),
                          s(c3, C2, b(T1,F1),b(T2,F2),b(F6,T6),b(F2,T2),b(F8,T8),b(T6,F6),b(F1,T1),b(T8,F8),b(T9,F9),b(T10,F10),b(T11,F11),b(T12,F12)))).

cnf(next_st, axiom, st_eq(s(next(c3), C2, b(T1,F1), b(T2,F2), b(T3,F3), b(T4,F4), b(T5,F5), b(T6,F6), b(T7,F7), b(T8,F8), b(T9,F9), b(T10,F10), b(T11,F11), b(T12,F12)),
                          s(c4, C2, b(F6,T6),b(F9,T9),b(F8,T8),b(T4,F4),b(F7,T7),b(T6,F6),b(T7,F7),b(T8,F8),b(T9,F9),b(T10,F10),b(T11,F11),b(T12,F12)))).

cnf(next_st, axiom, st_eq(s(next(c4), C2, b(T1,F1), b(T2,F2), b(T3,F3), b(T4,F4), b(T5,F5), b(T6,F6), b(T7,F7), b(T8,F8), b(T9,F9), b(T10,F10), b(T11,F11), b(T12,F12)),
                          s(c5, C2, b(T1,F1),b(T2,F2),b(F1,T1),b(F5,T5),b(T5,F5),b(T6,F6),b(F6,T6),b(F9,T9),b(T9,F9),b(T10,F10),b(T11,F11),b(T12,F12)))).

cnf(next_st, axiom, st_eq(s(next(c5), C2, b(T1,F1), b(T2,F2), b(T3,F3), b(T4,F4), b(T5,F5), b(T6,F6), b(T7,F7), b(T8,F8), b(T9,F9), b(T10,F10), b(T11,F11), b(T12,F12)),
                          s(c0, C2, b(F8,T8),b(T2,F2),b(T3,F3),b(T4,F4),b(T5,F5),b(F4,T4),b(F3,T3),b(T8,F8),b(F2,T2),b(T10,F10),b(T11,F11),b(T12,F12)))).

cnf(next_st, axiom, st_eq(s(C1, next(c0), b(T1,F1), b(T2,F2), b(T3,F3), b(T4,F4), b(T5,F5), b(T6,F6), b(T7,F7), b(T8,F8), b(T9,F9), b(T10,F10), b(T11,F11), b(T12,F12)),
                          s(C1, c1, b(F4,T4),b(T2,F2),b(F2,T2),b(T4,F4),b(T5,F5),b(T6,F6),b(T7,F7),b(T8,F8),b(T9,F9),b(F11,T11),b(T11,F11),b(F6,T6)))).

cnf(next_st, axiom, st_eq(s(C1, next(c1), b(T1,F1), b(T2,F2), b(T3,F3), b(T4,F4), b(T5,F5), b(T6,F6), b(T7,F7), b(T8,F8), b(T9,F9), b(T10,F10), b(T11,F11), b(T12,F12)),
                          s(C1, c2, b(T1,F1),b(T2,F2),b(F11,T11),b(T4,F4),b(F1,T1),b(T6,F6),b(T7,F7),b(T8,F8),b(T9,F9),b(F6,T6),b(T11,F11),b(F2,T2)))).

cnf(next_st, axiom, st_eq(s(C1, next(c2), b(T1,F1), b(T2,F2), b(T3,F3), b(T4,F4), b(T5,F5), b(T6,F6), b(T7,F7), b(T8,F8), b(T9,F9), b(T10,F10), b(T11,F11), b(T12,F12)),
                          s(C1, c3, b(F11,T11),b(T2,F2),b(F4,T4),b(T4,F4),b(F10,T10),b(F2,T2),b(T7,F7),b(T8,F8),b(T9,F9),b(T10,F10),b(T11,F11),b(T12,F12)))).

cnf(next_st, axiom, st_eq(s(C1, next(c3), b(T1,F1), b(T2,F2), b(T3,F3), b(T4,F4), b(T5,F5), b(T6,F6), b(T7,F7), b(T8,F8), b(T9,F9), b(T10,F10), b(T11,F11), b(T12,F12)),
                          s(C1, c4, b(T1,F1),b(T2,F2),b(T3,F3),b(F3,T3),b(T5,F5),b(T6,F6),b(T7,F7),b(T8,F8),b(T9,F9),b(F6,T6),b(F5,T5),b(F1,T1)))).

cnf(next_st, axiom, st_eq(s(C1, next(c4), b(T1,F1), b(T2,F2), b(T3,F3), b(T4,F4), b(T5,F5), b(T6,F6), b(T7,F7), b(T8,F8), b(T9,F9), b(T10,F10), b(T11,F11), b(T12,F12)),
                          s(C1, c5, b(T1,F1),b(F3,T3),b(T3,F3),b(T4,F4),b(T5,F5),b(F1,T1),b(T7,F7),b(T8,F8),b(T9,F9),b(F5,T5),b(F12,T12),b(T12,F12)))).

cnf(next_st, axiom, st_eq(s(C1, next(c5), b(T1,F1), b(T2,F2), b(T3,F3), b(T4,F4), b(T5,F5), b(T6,F6), b(T7,F7), b(T8,F8), b(T9,F9), b(T10,F10), b(T11,F11), b(T12,F12)),
                          s(C1, c0, b(F11,T11),b(T2,F2),b(F2,T2),b(T4,F4),b(T5,F5),b(F4,T4),b(T7,F7),b(T8,F8),b(T9,F9),b(T10,F10),b(T11,F11),b(F5,T5)))).




cnf(check, negated_conjecture, pi0(ag(or(p1, or(p2, or(p3, or(p4, or(p5, p6)))))), s(c0, c0, b(tt, ff), b(ff, tt), b(tt, ff), b(tt, ff), b(tt, ff), b(tt, ff), b(ff, tt), b(ff, tt), b(ff, tt), b(ff, tt), b(ff, tt), b(ff, tt)))).


