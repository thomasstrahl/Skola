var 0.0..6000.0: B;var 0.0..4000.0: T;constraint (1.0/200.0)*B+(1.0/140.0)*T <= 40.0;solve maximize 25.0*B + 30.0*T;output  ["B = ", show(B), "T = ", show(T)];
