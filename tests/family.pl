zena(vdova).
zena(dcera_vdovy).

muz(ja).
muz(muj_otec).
muz(syn_dcery_vdovy).
muz(nas_syn).

manzel(ja, vdova).
manzel(muj_otec, dcera_vdovy).

manzele(Prvni, Druhy) :- manzel(Prvni, Druhy).
manzele(Prvni, Druhy) :- manzel(Druhy, Prvni).

syn(ja, muj_otec).
syn(nas_syn, ja).
syn(syn_dcery_vdovy, dcera_vdovy).

dcera(dcera_vdovy, vdova).

dite(Dite, Rodic) :- syn(Dite, Rodic).
dite(Dite, Rodic) :- dcera(Dite, Rodic).

matka(Matka, Dite) :- zena(Matka), dite(Dite, Matka).
matka(Matka, Dite) :- nmatka(Matka, Dite).
nmatka(NMatka, Dite) :- zena(NMatka), muz(Otec), dite(Dite, Otec), manzele(Otec, NMatka).

otec(Otec, Dite) :- muz(Otec), dite(Dite, Otec).
otec(Otec, Dite) :- notec(Otec, Dite).
notec(NOtec, Dite) :- muz(NOtec), zena(Matka), dite(Dite, Matka), manzele(NOtec, Matka).

rodic(Rodic, Dite) :- otec(Rodic, Dite).
rodic(Rodic, Dite) :- matka(Rodic, Dite).

zet(Zet, Rodic) :- muz(Zet), manzele(Zet, Dcera), rodic(Rodic, Dcera).

bratr(Kdo, Ci) :- muz(Kdo), rodic(Rodic, Kdo), rodic(Ci, Rodic).

stryc(Kdo, Ci) :- rodic(Rodic, Ci), bratr(Kdo, Rodic).

vnuk(Kdo, Ci) :- muz(Kdo), rodic(Rodic, Kdo), rodic(Ci, Rodic).

dedecek(Kdo, Ci) :- muz(Kdo), vnuk(Ci, Kdo).
