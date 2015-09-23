% RUNS ON mzn_tc
%-----------------------------------------------------------------------------%
% This file tests built-in operations and annotations.
%-----------------------------------------------------------------------------%
par   int: pi = 3;
par float: pf = 3.0;
par bool:  pb = true;

par set of int:   psi = {1};
par set of float: psf = {1.3};
par set of bool:  psb = {true};

var int: vi;
var 1..3: vi123;

var float: vf;
var 1.0..3.0: vf123;

var bool: vb;

par string: ps = "foo";

var set of 1..3: vsi;
var set of 1..3: vsi123;

array[0..5] of par bool:        arr_pb = [true,false,true,false,true];
array[0..5] of var bool:        arr_vb;
array[0..5] of par int:         arr_pi = [1,2,3,4,5];
array[0..5] of var int:         arr_vi;
array[0..5] of var 1..3:        arr_vi123 = [1,2,3,1,2];
array[0..5] of var 1.0..3.0:    arr_vf123 = [1.0,2.1,2.9,1.6,2.8];
array[0..5] of par float:       arr_pf = [1.0, 2.0, 3.0, 4.0, 5.0];
array[0..5] of var float:       arr_vf;
array[0..5] of par set of int:  arr_psi = [{1}, {2}, {3}, {4}, {5}];
array[0..1] of par set of bool: arr_psb = [{true}, {false}];
array[0..1] of par set of float:arr_psf = [{1.1}, {2.2}];
array[0..5] of var set of 1..3: arr_vsi123;
array[0..1,0..1] of int:        arr_2_pi = [|1, 2 | 3, 4|];
array[0..1,0..1,0..1] of int:   arr_3_pi =
    array3d(1..2, 1..2, 1..2, [1,2,3,4,5,6,7,8]);
array[0..1,0..1,0..1,0..1] of int: arr_4_pi =
    array4d(1..2, 1..2, 1..2, 1..2, [i | i in 1..16]);
array[0..1,0..1,0..1,0..1,0..1] of int: arr_5_pi =
    array5d(1..2, 1..2, 1..2, 1..2, 1..2, [i | i in 1..32]);
array[0..1,0..1,0..1,0..1,0..1,0..1] of int: arr_6_pi =
    array6d(1..2, 1..2, 1..2, 1..2, 1..2, 1..2, [i | i in 1..64]);

%-----------------------------------------------------------------------------%
% Comparisons
%-----------------------------------------------------------------------------%

par bool: a00 = pi == pi;
var bool: a01 = pi == vi;
var bool: a02 = vi == pi;
var bool: a03 = vi == vi;

par bool: a10 = pi =  pi;
var bool: a11 = pi =  vi;
var bool: a12 = vi =  pi;
var bool: a13 = vi =  vi;

par bool: a20 = pi != pi;
var bool: a21 = pi != vi;
var bool: a22 = vi != pi;
var bool: a23 = vi != vi;

par bool: a30 = pi <= pi;
var bool: a31 = pi <= vi;
var bool: a32 = vi <= pi;
var bool: a33 = vi <= vi;

par bool: a40 = pi <  pi;
var bool: a41 = pi <  vi;
var bool: a42 = vi <  pi;
var bool: a43 = vi <  vi;

par bool: a50 = pi >= pi;
var bool: a51 = pi >= vi;
var bool: a52 = vi >= pi;
var bool: a53 = vi >= vi;

par bool: a60 = pi >  pi;
var bool: a61 = pi >  vi;
var bool: a62 = vi >  pi;
var bool: a63 = vi >  vi;

%-----------------------------------------------------------------------------%
% Arithmetic
%-----------------------------------------------------------------------------%

par int: b00 = pi +   pi;
var int: b01 = pi +   vi;
var int: b02 = vi +   pi;
var int: b03 = vi +   vi;

par float: b04 = pf + pf;
var float: b05 = pf + vf;
var float: b06 = vf + pf;
var float: b07 = vf + vf;

par int: b08 = +pi;
var int: b09 = +vi;

par float: b10 = +pf;
var float: b11 = +vf;

par int: b12 = pi -   pi;
var int: b13 = pi -   vi;
var int: b14 = vi -   pi;
var int: b15 = vi -   vi;

par float: b16 = pf - pf;
var float: b17 = pf - vf;
var float: b18 = vf - pf;
var float: b19 = vf - vf;

par int: b20 = -pi;
var int: b21 = -vi;

par float: b22 = -pf;
var float: b23 = -vf;

par int: b24 = pi *   pi;
var int: b25 = pi *   vi;
var int: b26 = vi *   pi;
var int: b27 = vi *   vi;

par float: b28 = pf * pf;
var float: b29 = pf * vf;
var float: b30 = vf * pf;
var float: b31 = vf * vf;

par int: b32 = pi div pi;
var int: b33 = pi div vi;
var int: b34 = vi div pi;
var int: b35 = vi div vi;

par int: b36 = pi mod pi;
var int: b37 = pi mod vi;
var int: b38 = vi mod pi;
var int: b39 = vi mod vi;

par float: b40 = pf / pf;
var float: b41 = pf / vf;
var float: b42 = vf / pf;
var float: b43 = vf / vf;

par int:    b50 = sum(arr_pi);
var int:    b51 = sum(arr_vi);
par float:  b52 = sum(arr_pf);
var float:  b53 = sum(arr_vf);

par int:    b54 = product(arr_pi);
var int:    b55 = product(arr_vi);
par float:  b56 = product(arr_pf);
var float:  b57 = product(arr_vf);

par int:    b60 = min(pi, pi);
par int:    b61 = min([ pi, pi ]);
par int:    b62 = min({ pi, pi });
par float:  b63 = min(pf, pf);
par float:  b64 = min([ pf, pf ]);
par float:  b65 = min({ pf, pf });

par int:    b70 = max(pi, pi);
par int:    b71 = max([ pi, pi ]);
par int:    b72 = max({ pi, pi });
par float:  b73 = max(pf, pf);
par float:  b74 = max([ pf, pf ]);
par float:  b75 = max({ pf, pf });

par int:    b80 = abs(pi);
var int:    b81 = abs(vi);
par float:  b82 = abs(pf);
var float:  b83 = abs(vf);

par float:  b90 = sqrt (pf);
par int:    b91 = pow(pi, pi);
par float:  b92 = pow(pf, pf);
par float:  b93 = ln   (pf);
par float:  b94 = log10(pf);
par float:  b95 = log2 (pf);
par float:  b96 = log  (pf, pf);
par float:  b97 = sin  (pf);
par float:  b98 = cos  (pf);
par float:  b99 = tan  (pf);
par float:  b100= asin (pf);
par float:  b101= acos (pf);
par float:  b102= atan (pf);
par float:  b103= sinh (pf);
par float:  b104= cosh (pf);
par float:  b105= tanh (pf);

%-----------------------------------------------------------------------------%
% Logic
%-----------------------------------------------------------------------------%

par bool: c00 = pb <-> pb;
var bool: c01 = pb <-> vb;
var bool: c02 = vb <-> pb;
var bool: c03 = vb <-> vb;

par bool: c10 = pb <-  pb;
var bool: c11 = pb <-  vb;
var bool: c12 = vb <-  pb;
var bool: c13 = vb <-  vb;

par bool: c14 = pb ->  pb;
var bool: c15 = pb ->  vb;
var bool: c16 = vb ->  pb;
var bool: c17 = vb ->  vb;

par bool: c20 = pb \/  pb;
var bool: c21 = pb \/  vb;
var bool: c22 = vb \/  pb;
var bool: c23 = vb \/  vb;

par bool: c24 = pb /\  pb;
var bool: c25 = pb /\  vb;
var bool: c26 = vb /\  pb;
var bool: c27 = vb /\  vb;

par bool: c30 = pb xor pb;
var bool: c31 = pb xor vb;
var bool: c32 = vb xor pb;
var bool: c33 = vb xor vb;

par bool: c40 = not pb;
var bool: c41 = not vb;

par bool: c50 = forall(arr_pb);
var bool: c51 = forall(arr_vb);

par bool: c60 = exists(arr_pb);
var bool: c61 = exists(arr_vb);

%-----------------------------------------------------------------------------%
% Sets
%-----------------------------------------------------------------------------%

par bool: d00 = pi in psi;
var bool: d01 = pi in vsi;
var bool: d02 = vi in psi;
var bool: d03 = vi in vsi;

par bool: d10 = psi subset   psi;
var bool: d11 = psi subset   vsi;
var bool: d12 = vsi subset   psi;
var bool: d13 = vsi subset   vsi;

par bool: d14 = psi superset psi;
var bool: d15 = psi superset vsi;
var bool: d16 = vsi superset psi;
var bool: d17 = vsi superset vsi;

par set of 1..3: d20 = psi union     psi;
var set of 1..3: d21 = psi union     vsi;
var set of 1..3: d22 = vsi union     psi;
var set of 1..3: d23 = vsi union     vsi;

par set of 1..3: d24 = psi intersect psi;
var set of 1..3: d25 = psi intersect vsi;
var set of 1..3: d26 = vsi intersect psi;
var set of 1..3: d27 = vsi intersect vsi;

par set of 1..3: d30 = psi diff      psi;
var set of 1..3: d31 = psi diff      vsi;
var set of 1..3: d32 = vsi diff      psi;
var set of 1..3: d33 = vsi diff      vsi;

par set of 1..3: d34 = psi symdiff   psi;
var set of 1..3: d35 = psi symdiff   vsi;
var set of 1..3: d36 = vsi symdiff   psi;
var set of 1..3: d37 = vsi symdiff   vsi;

par set of 1..3: d40 = pi .. pi;

par int: d50 = card(psi);
par int: d51 = card(psb);
par int: d52 = card(psf);
var int: d53 = card(vsi);

par set of int:     d60 = array_union(arr_psi);
par set of float:   d61 = array_union(arr_psf);
par set of bool:    d62 = array_union(arr_psb);
var set of 1..3:    d63 = array_union(arr_vsi123);

par set of int:     d64 = array_intersect(arr_psi);
par set of float:   d65 = array_intersect(arr_psf);
par set of bool:    d66 = array_intersect(arr_psb);
var set of 1..3:    d67 = array_intersect(arr_vsi123);

%-----------------------------------------------------------------------------%
% Arrays
%-----------------------------------------------------------------------------%

int:            e00 = length(arr_pi);
int:            e01 = length(arr_vf);

array[1..3] of par int: e10 = arr_pi ++ arr_pi;
array[1..3] of var int: e11 = arr_pi ++ arr_vi;
array[1..3] of var int: e12 = arr_vi ++ arr_pi;
array[1..3] of var int: e13 = arr_vi ++ arr_vi;

set of int:     e20 = index_set(arr_pi);
set of int:     e21 = index_set(arr_vf);
set of int:     e22 = index_set_1of2(arr_2_pi);
set of int:     e23 = index_set_2of2(arr_2_pi);
set of int:     e24 = index_set_1of3(arr_3_pi);
set of int:     e25 = index_set_2of3(arr_3_pi);
set of int:     e26 = index_set_3of3(arr_3_pi);

array[1..3]           of par int: e30 = array1d(psi,           arr_pi);
array[1..3,1..3]      of par int: e31 = array2d(psi, psi,      arr_pi);
array[1..3,1..3,1..3] of par int: e32 = array3d(psi, psi, psi, arr_pi);

%-----------------------------------------------------------------------------%
% Coercions
%-----------------------------------------------------------------------------%

par int:    f00 = ceil(pf);
par int:    f01 = floor(pf);
par int:    f02 = round(pf);

par int:    i10 = bool2int(pb);
var int:    i11 = bool2int(vb);

par float:  i20 = int2float(pi);
var float:  i21 = int2float(vi);

array[0..3] of par int:   i30 = set2array(psi);
array[0..3] of par float: i31 = set2array(psf);
array[0..3] of par bool:  i32 = set2array(psb);

%-----------------------------------------------------------------------------%
% Strings
%-----------------------------------------------------------------------------%

par string: j00 = ps ++ ps;

par string: j10 = show(pf);
par string: j11 = show(vi);
par string: j12 = show(vsi);
par string: j13 = show(arr_vsi123);

%-----------------------------------------------------------------------------%
% Bound and Domain Operations
%-----------------------------------------------------------------------------%

par int:        k10 = lb(    vi123);
par int:        k11 = lb_array(arr_vi123);
par float:      k12 = lb(    vf123);
par float:      k13 = lb_array(arr_vf123);

par int:        k20 = ub(    vi123);
par int:        k21 = ub_array(arr_vi123);
par float:      k22 = ub(    vf123);
par float:      k23 = ub_array(arr_vf123);
set of int:     k24 = ub(    vsi123);
set of int:     k25 = ub_array(arr_vsi123);

set of par int: k30 = dom(    vi123);
set of par int: k31 = dom_array(arr_vi123);

%-----------------------------------------------------------------------------%
% Other
%-----------------------------------------------------------------------------%

par int:        k00 = assert(pb, "s", pi);
var set of 1..3:k01 = assert(pb, "s", vsi);
var set of 1..3:k02 = assert(pb, "s", _);
par bool:       k03 = assert(pb, "s");   

par int:        k40 = abort("foo");
par int:        k41 = if true then 3 else abort("foo") endif;

var int:        k50;
par int:        k51 = fix(k50);
par int:        k52 = fix(k51);
array[1..5] of var int: k53;
array[1..5] of par int: k54 = fix(k53);

% first, second, foldl, foldr

%-----------------------------------------------------------------------------%
% Variable annotations
%-----------------------------------------------------------------------------%

var int: vi4 :: bounds;
var int: vi5 :: bitmap(1,10);
var int: vi6 :: cardinality;

%-----------------------------------------------------------------------------%
% Constraint annotations
%-----------------------------------------------------------------------------%

constraint '<'(vi, vi) :: bounds;
constraint '<'(vi, vi) :: boundsZ;
constraint '<'(vi, vi) :: boundsR;
constraint '<'(vi, vi) :: boundsD;
constraint '<'(vi, vi) :: domain;
constraint '<'(vi, vi) :: priority(3);
constraint '<'(vi, vi) :: multiple(["prop1", "prop2"]);
constraint '<'(vi, vi) :: staged(["sprop3", "sprop4"]);

%-----------------------------------------------------------------------------%
% Solve annotations
%-----------------------------------------------------------------------------%

solve :: int_search([vi4, vi5, vi6], first_fail, indomain, complete)
      :: bool_search( [vb, vb],       input_order, indomain_min, complete)
      :: float_search([vf, vf], 0.01, input_order, indomain_split, complete)
      :: set_search(  [vsi, vsi],     input_order, indomain_min, complete)
      :: labelling_ff
      satisfy;

