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

cnf(v1f, axiom, ~pi0(v1, s(CP, P1_CP, b(ff,tt), V2, V3, V4, V5, V6, V7, V8, V9, P1_V10, P1_V11, P1_V12))).
cnf(v1t, axiom,  pi0(v1, s(CP, P1_CP, b(tt,ff), V2, V3, V4, V5, V6, V7, V8, V9, P1_V10, P1_V11, P1_V12))).
cnf(v2f, axiom, ~pi0(v2, s(CP, P1_CP, V1, b(ff,tt), V3, V4, V5, V6, V7, V8, V9, P1_V10, P1_V11, P1_V12))).
cnf(v2t, axiom,  pi0(v2, s(CP, P1_CP, V1, b(tt,ff), V3, V4, V5, V6, V7, V8, V9, P1_V10, P1_V11, P1_V12))).
cnf(v3f, axiom, ~pi0(v3, s(CP, P1_CP, V1, V2, b(ff,tt), V4, V5, V6, V7, V8, V9, P1_V10, P1_V11, P1_V12))).
cnf(v3t, axiom,  pi0(v3, s(CP, P1_CP, V1, V2, b(tt,ff), V4, V5, V6, V7, V8, V9, P1_V10, P1_V11, P1_V12))).
cnf(v4f, axiom, ~pi0(v4, s(CP, P1_CP, V1, V2, V3, b(ff,tt), V5, V6, V7, V8, V9, P1_V10, P1_V11, P1_V12))).
cnf(v4t, axiom,  pi0(v4, s(CP, P1_CP, V1, V2, V3, b(tt,ff), V5, V6, V7, V8, V9, P1_V10, P1_V11, P1_V12))).
cnf(v5f, axiom, ~pi0(v5, s(CP, P1_CP, V1, V2, V3, V4, b(ff,tt), V6, V7, V8, V9, P1_V10, P1_V11, P1_V12))).
cnf(v5t, axiom,  pi0(v5, s(CP, P1_CP, V1, V2, V3, V4, b(tt,ff), V6, V7, V8, V9, P1_V10, P1_V11, P1_V12))).
cnf(v6f, axiom, ~pi0(v6, s(CP, P1_CP, V1, V2, V3, V4, V5, b(ff,tt), V7, V8, V9, P1_V10, P1_V11, P1_V12))).
cnf(v6t, axiom,  pi0(v6, s(CP, P1_CP, V1, V2, V3, V4, V5, b(tt,ff), V7, V8, V9, P1_V10, P1_V11, P1_V12))).
cnf(v7f, axiom, ~pi0(v7, s(CP, P1_CP, V1, V2, V3, V4, V5, V6, b(ff,tt), V8, V9, P1_V10, P1_V11, P1_V12))).
cnf(v7t, axiom,  pi0(v7, s(CP, P1_CP, V1, V2, V3, V4, V5, V6, b(tt,ff), V8, V9, P1_V10, P1_V11, P1_V12))).
cnf(v8f, axiom, ~pi0(v8, s(CP, P1_CP, V1, V2, V3, V4, V5, V6, V7, b(ff,tt), V9, P1_V10, P1_V11, P1_V12))).
cnf(v8t, axiom,  pi0(v8, s(CP, P1_CP, V1, V2, V3, V4, V5, V6, V7, b(tt,ff), V9, P1_V10, P1_V11, P1_V12))).
cnf(v9f, axiom, ~pi0(v9, s(CP, P1_CP, V1, V2, V3, V4, V5, V6, V7, V8, b(ff,tt), P1_V10, P1_V11, P1_V12))).
cnf(v9t, axiom,  pi0(v9, s(CP, P1_CP, V1, V2, V3, V4, V5, V6, V7, V8, b(tt,ff), P1_V10, P1_V11, P1_V12))).


cnf(r, axiom, r(s(CP, P1_CP, b(Tv1,Fv1), b(Tv2,Fv2), b(Tv3,Fv3), b(Tv4,Fv4), b(Tv5,Fv5), b(Tv6,Fv6), b(Tv7,Fv7), b(Tv8,Fv8), b(Tv9,Fv9), b(Tp1_v10,Fp1_v10), b(Tp1_v11,Fp1_v11), b(Tp1_v12,Fp1_v12) ), con(S0, con(S1, nil)))
              | ~st_eq(s(next(CP), P1_CP, b(Tv1,Fv1), b(Tv2,Fv2), b(Tv3,Fv3), b(Tv4,Fv4), b(Tv5,Fv5), b(Tv6,Fv6), b(Tv7,Fv7), b(Tv8,Fv8), b(Tv9,Fv9), b(Tp1_v10,Fp1_v10), b(Tp1_v11,Fp1_v11), b(Tp1_v12,Fp1_v12) ), S0)
              | ~st_eq(s(CP, next(P1_CP), b(Tv1,Fv1), b(Tv2,Fv2), b(Tv3,Fv3), b(Tv4,Fv4), b(Tv5,Fv5), b(Tv6,Fv6), b(Tv7,Fv7), b(Tv8,Fv8), b(Tv9,Fv9), b(Tp1_v10,Fp1_v10), b(Tp1_v11,Fp1_v11), b(Tp1_v12,Fp1_v12) ), S1)).

cnf(next_st, axiom, st_eq(s(next(c0), P1_CP, b(Tv1,Fv1), b(Tv2,Fv2), b(Tv3,Fv3), b(Tv4,Fv4), b(Tv5,Fv5), b(Tv6,Fv6), b(Tv7,Fv7), b(Tv8,Fv8), b(Tv9,Fv9), b(Tp1_v10,Fp1_v10), b(Tp1_v11,Fp1_v11), b(Tp1_v12,Fp1_v12) ),
          s(c1, P1_CP, b(Fv9,Tv9), b(Fv4,Tv4), b(Tv3,Fv3), b(Tv4,Fv4), b(Fv8,Tv8), b(Tv6,Fv6), b(Fv6,Tv6), b(Tv8,Fv8), b(Tv9,Fv9), P1_V10, P1_V11, P1_V12))).

cnf(next_st, axiom, st_eq(s(next(c1), P1_CP, b(Tv1,Fv1), b(Tv2,Fv2), b(Tv3,Fv3), b(Tv4,Fv4), b(Tv5,Fv5), b(Tv6,Fv6), b(Tv7,Fv7), b(Tv8,Fv8), b(Tv9,Fv9), b(Tp1_v10,Fp1_v10), b(Tp1_v11,Fp1_v11), b(Tp1_v12,Fp1_v12) ),
          s(c2, P1_CP, b(Tv1,Fv1), b(Tv2,Fv2), b(Tv3,Fv3), b(Fv8,Tv8), b(Fv2,Tv2), b(Fv1,Tv1), b(Tv7,Fv7), b(Tv8,Fv8), b(Fv7,Tv7), P1_V10, P1_V11, P1_V12))).

cnf(next_st, axiom, st_eq(s(next(c2), P1_CP, b(Tv1,Fv1), b(Tv2,Fv2), b(Tv3,Fv3), b(Tv4,Fv4), b(Tv5,Fv5), b(Tv6,Fv6), b(Tv7,Fv7), b(Tv8,Fv8), b(Tv9,Fv9), b(Tp1_v10,Fp1_v10), b(Tp1_v11,Fp1_v11), b(Tp1_v12,Fp1_v12) ),
          s(c3, P1_CP, b(Tv1,Fv1), b(Tv2,Fv2), b(Fv6,Tv6), b(Fv2,Tv2), b(Fv8,Tv8), b(Tv6,Fv6), b(Fv1,Tv1), b(Tv8,Fv8), b(Tv9,Fv9), P1_V10, P1_V11, P1_V12))).

cnf(next_st, axiom, st_eq(s(next(c3), P1_CP, b(Tv1,Fv1), b(Tv2,Fv2), b(Tv3,Fv3), b(Tv4,Fv4), b(Tv5,Fv5), b(Tv6,Fv6), b(Tv7,Fv7), b(Tv8,Fv8), b(Tv9,Fv9), b(Tp1_v10,Fp1_v10), b(Tp1_v11,Fp1_v11), b(Tp1_v12,Fp1_v12) ),
          s(c4, P1_CP, b(Fv6,Tv6), b(Fv9,Tv9), b(Fv8,Tv8), b(Tv4,Fv4), b(Fv7,Tv7), b(Tv6,Fv6), b(Tv7,Fv7), b(Tv8,Fv8), b(Tv9,Fv9), P1_V10, P1_V11, P1_V12))).

cnf(next_st, axiom, st_eq(s(next(c4), P1_CP, b(Tv1,Fv1), b(Tv2,Fv2), b(Tv3,Fv3), b(Tv4,Fv4), b(Tv5,Fv5), b(Tv6,Fv6), b(Tv7,Fv7), b(Tv8,Fv8), b(Tv9,Fv9), b(Tp1_v10,Fp1_v10), b(Tp1_v11,Fp1_v11), b(Tp1_v12,Fp1_v12) ),
          s(c5, P1_CP, b(Tv1,Fv1), b(Tv2,Fv2), b(Fv1,Tv1), b(Fv5,Tv5), b(Tv5,Fv5), b(Tv6,Fv6), b(Fv6,Tv6), b(Fv9,Tv9), b(Tv9,Fv9), P1_V10, P1_V11, P1_V12))).

cnf(next_st, axiom, st_eq(s(next(c5), P1_CP, b(Tv1,Fv1), b(Tv2,Fv2), b(Tv3,Fv3), b(Tv4,Fv4), b(Tv5,Fv5), b(Tv6,Fv6), b(Tv7,Fv7), b(Tv8,Fv8), b(Tv9,Fv9), b(Tp1_v10,Fp1_v10), b(Tp1_v11,Fp1_v11), b(Tp1_v12,Fp1_v12) ),
          s(c0, P1_CP, b(Fv8,Tv8), b(Tv2,Fv2), b(Tv3,Fv3), b(Tv4,Fv4), b(Tv5,Fv5), b(Fv4,Tv4), b(Fv3,Tv3), b(Tv8,Fv8), b(Fv2,Tv2), P1_V10, P1_V11, P1_V12))).

cnf(next_st, axiom, st_eq(s(CP, next(c0), b(Tv1,Fv1), b(Tv2,Fv2), b(Tv3,Fv3), b(Tv4,Fv4), b(Tv5,Fv5), b(Tv6,Fv6), b(Tv7,Fv7), b(Tv8,Fv8), b(Tv9,Fv9), b(Tp1_v10,Fp1_v10), b(Tp1_v11,Fp1_v11), b(Tp1_v12,Fp1_v12) ),
          s(CP, c1, b(Fv4,Tv4), b(Tv2,Fv2), b(Fv2,Tv2), b(Tv4,Fv4), b(Tv5,Fv5), b(Tv6,Fv6), V7, V8, V9, b(Fp1_v11,Tp1_v11), b(Tp1_v11,Fp1_v11), b(Fv6,Tv6)))).

cnf(next_st, axiom, st_eq(s(CP, next(c1), b(Tv1,Fv1), b(Tv2,Fv2), b(Tv3,Fv3), b(Tv4,Fv4), b(Tv5,Fv5), b(Tv6,Fv6), b(Tv7,Fv7), b(Tv8,Fv8), b(Tv9,Fv9), b(Tp1_v10,Fp1_v10), b(Tp1_v11,Fp1_v11), b(Tp1_v12,Fp1_v12) ),
          s(CP, c2, b(Tv1,Fv1), b(Tv2,Fv2), b(Fp1_v11,Tp1_v11), b(Tv4,Fv4), b(Fv1,Tv1), b(Tv6,Fv6), V7, V8, V9, b(Fv6,Tv6), b(Tp1_v11,Fp1_v11), b(Fv2,Tv2)))).

cnf(next_st, axiom, st_eq(s(CP, next(c2), b(Tv1,Fv1), b(Tv2,Fv2), b(Tv3,Fv3), b(Tv4,Fv4), b(Tv5,Fv5), b(Tv6,Fv6), b(Tv7,Fv7), b(Tv8,Fv8), b(Tv9,Fv9), b(Tp1_v10,Fp1_v10), b(Tp1_v11,Fp1_v11), b(Tp1_v12,Fp1_v12) ),
          s(CP, c3, b(Fp1_v11,Tp1_v11), b(Tv2,Fv2), b(Fv4,Tv4), b(Tv4,Fv4), b(Fp1_v10,Tp1_v10), b(Fv2,Tv2), V7, V8, V9, b(Tp1_v10,Fp1_v10), b(Tp1_v11,Fp1_v11), b(Tp1_v12,Fp1_v12)))).

cnf(next_st, axiom, st_eq(s(CP, next(c3), b(Tv1,Fv1), b(Tv2,Fv2), b(Tv3,Fv3), b(Tv4,Fv4), b(Tv5,Fv5), b(Tv6,Fv6), b(Tv7,Fv7), b(Tv8,Fv8), b(Tv9,Fv9), b(Tp1_v10,Fp1_v10), b(Tp1_v11,Fp1_v11), b(Tp1_v12,Fp1_v12) ),
          s(CP, c4, b(Tv1,Fv1), b(Tv2,Fv2), b(Tv3,Fv3), b(Fv3,Tv3), b(Tv5,Fv5), b(Tv6,Fv6), V7, V8, V9, b(Fv6,Tv6), b(Fv5,Tv5), b(Fv1,Tv1)))).

cnf(next_st, axiom, st_eq(s(CP, next(c4), b(Tv1,Fv1), b(Tv2,Fv2), b(Tv3,Fv3), b(Tv4,Fv4), b(Tv5,Fv5), b(Tv6,Fv6), b(Tv7,Fv7), b(Tv8,Fv8), b(Tv9,Fv9), b(Tp1_v10,Fp1_v10), b(Tp1_v11,Fp1_v11), b(Tp1_v12,Fp1_v12) ),
          s(CP, c5, b(Tv1,Fv1), b(Fv3,Tv3), b(Tv3,Fv3), b(Tv4,Fv4), b(Tv5,Fv5), b(Fv1,Tv1), V7, V8, V9, b(Fv5,Tv5), b(Fp1_v12,Tp1_v12), b(Tp1_v12,Fp1_v12)))).

cnf(next_st, axiom, st_eq(s(CP, next(c5), b(Tv1,Fv1), b(Tv2,Fv2), b(Tv3,Fv3), b(Tv4,Fv4), b(Tv5,Fv5), b(Tv6,Fv6), b(Tv7,Fv7), b(Tv8,Fv8), b(Tv9,Fv9), b(Tp1_v10,Fp1_v10), b(Tp1_v11,Fp1_v11), b(Tp1_v12,Fp1_v12) ),
          s(CP, c0, b(Fp1_v11,Tp1_v11), b(Tv2,Fv2), b(Fv2,Tv2), b(Tv4,Fv4), b(Tv5,Fv5), b(Fv4,Tv4), V7, V8, V9, b(Tp1_v10,Fp1_v10), b(Tp1_v11,Fp1_v11), b(Fv5,Tv5)))).


cnf(check, negated_conjecture, pi0(ag(or(v1,or(v2,or(v3,or(v4,or(v5,v6)))))),
         s(c0, c0, b(tt,ff), b(ff,tt), b(tt,ff), b(tt,ff), b(tt,ff), b(tt,ff), b(ff,tt), b(ff,tt), b(ff,tt), b(ff,tt), b(ff,tt), b(ff,tt)))).
