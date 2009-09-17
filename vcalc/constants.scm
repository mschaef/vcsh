;;;; constants.scm
;;;; Mike Schaeffer
;;
;; A library of standard constants

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vCalc Constants

; A list of constants. Entries take the form:
;
; ( <name> <value> <unit-string>/"" <relative-error>/:exact )

(define *constant-library* 
  '(
    ;; General
    ("Speed of light in Vacuum" 299792458 "m s-1" :exact)
    ("Magnetic constant" 12.566370614e-7 "N A-2" :exact)
    ("Electric constant" 8.854187817e-12 "F m-1" :exact)
    ("Characteristic impedance of vacuum" 376.730313461 "Ohms" :exact)
    ("Newtonian constant of gravitation (G)" 6.67310e-11 "m3 kg-1 s-2" 1.5e-3)
    ("Planck constant" 6.6260687652e-34 "J s" 7.8e-8)
    ("Planck mass" 2.176716e-8 "kg" 7.5e-4)
    ("Planck length" 1.616012e-35 "m" 7.5e-4)
    ("Planck time" 5.390640e-44 "s" 7.5e-4)

    ;; Electromagnetic
    ("Elementary charge" 1.60217646263e-19 "C" 3.9e-8)
    ("Elementary charge" 2.41798949195e14 "A J-1" 3.9e-8)
    ("Magnetic flux quantum" 2.06783363681e-15 "Wb" 3.9e-8)
    ("Conductance quantum" 7.748091696e-5 "S" 3.7e-9)
    ("Inverse of conductance quantum" 12906.40378647 "Ohms" 3.7e-9)
    ("Josephson constant" 483597.89819e9 "Hz V-1" 3.9e-8)
    ("von Klitzing constant" 25812.80757295 "Ohms" 3.7e-9)
    ("Bohr magneton" 927.40089937e-26 "J T-1" 4.0e-8)
    ("Nuclear magneton" 5.0507831720e-27 "J T-1" 4.0e-8)

    ;; Atomic and Nuclear, General
    ("Fine-structure constant" 7.29735253327e-3 "" 3.7e-9)
    ("Inverse fine-structure constant" 137.0359997650 "" 3.7e-9)
    ("Rydberg constant" 10973731.56854983 "m-1" 7.6e-12)
    ("Bohr radius" 0.529177208319e-10 "m" 3.7e-9)
    ("Hartree energy" 4.3597438134e-18 "J" 7.8e-8 )
    ("Hartree energy" 27.211383411 "eV" 3.9e-8)
    ("Quantum of circulation/2" 3.63694751627e-4 "m2 s-1" 7.3e-9)
    ("Quantum of circulation" 7.27389503253e-4 "m2 s-1" 7.3e-9)
    
    ;; Atomic and Nuclear, Electroweak
    ("Fermi coupling constant" 1.166391e-5 "GeV-2" 8.6e-6)
    ("Weak mixing angle" 0.222419 "" 8.7e-3)

    ;; Atomic and Nuclear, Electron
    ("Electron mass" 9.1093818872e-31 "kg" 7.9e-8)
    ("Electron mass energy equivalent" 8.1871041464e-14 "J" 7.9e-8)
    ("Electron mass energy equivalent" 0.51099890221 "MeV" 4.0e-8)
    ("Electron-muon mass ratio" 4.8363321015e-3 "" 3.0e-8)
    ("Electron-tau mass ratio" 2.8755547e-4 "" 1.6e-4)
    ("Electron-proton mass ratio" 5.44617023212e-4 "" 2.1e-9)
    ("Electron-neutron mass ratio" 5.43867346212e-4 "" 2.2e-9)
    ("Electron-deuteron mass ratio" 2.724437117058e-4 "" 2.1e-9)
    ("Electron-alpha particle mass ratio" 1.370933561129e-4 "" 2.1e-9)
    ("Electron charge to mass quotient" -1.75882017471e11 "C kg-1" 4.0e-8)
    ("Electron molar mass" 5.48579911012e-7 "kg mol-1" 2.1e-9)
    ("Electron Compton wavelength" 2.42631021518E-12 "m" 7.3e-9)
    ("Classical electron radius" 2.81794028531e-15 "m" 1.1e-8)
    ("Thomson cross section" 0.66524585415e-28 "m2" 2.2e-8)
    ("Electron magnetic moment" -928.47636237e-26 "J T-1" 4.0e-8)
    ("Electron magnetic moment to Bohr magneton ratio" -1.001159652186941 "" 4.1e-12)
    ("Electron magnetic moment to nuclear magneton ratio" -1.838281966039 "" 2.1e-9)
    ("Electron magnetic moment anomaly" 1.159652186941e-3 "" 3.5e-9)
    ("Electron g-factor" -2.002319304373782 "" 4.1e-12)
    ("Electron-muon magnetic moment ratio" 206.766972063 "" 3.0e-8)
    ("Electron-proton magnetic moment ratio" -658.210687566 "" 1.0e-8)
    ("Electron to shielded proton magnetic moment ratio" -658.227595471 "" 1.1e-8)
    ("Electron-neutron magnetic moment ratio" 960.9205023 "" 2.4e-7)
    ("Electron-deuteron magnetic moment ratio" -2143.92349823 "" 1.1e-8)
    ("Electron to shielded helion magnetic moment ratio" 864.05825510 "" 1.2e-8)
    ("Electron gyromagnetic ratio" 1.76085979471e11 "s-1 T-1" 4.0e-8)
    ("Electron gyromagnetic ratio" 28024.954011 "MHz T-1" 4.0e-8)
    
    ;; Atomic and Nuclear, Muon
    ("Muon mass" 1.8835310916e-28 "kg" 8.4e-8)
    ("Muon mass energy equivalent" 1.6928333214e-11 "J" 8.4e-8)
    ("Muon mass energy equivalent" 105.658356852 "MeV" 4.9e-8)
    ("Muon-electron mass ratio" 206.768265763 "" 3.0e-8)
    ("Muon-tau mass ratio" 5.9457297e-2 "" 1.6e-4)
    ("Muon-proton mass ratio" 0.112609517334 "" 3.0e-8)
    ("Muon-neutron mass ratio" 0.112454507934 "" 3.0e-8)
    ("Muon molar mass" 0.113428916834e-3 "kg mol-1" 3.0e-8)
    ("Muon Compton wavelength" 11.7344419735e-15 "m" 2.9e-8)
    ("Muon magnetic moment" -4.4904481322e-26 "J T-1" 4.9e-8)
    ("Muon magnetic moment to Bohr magneton ratio" -4.8419708515e-3 "" 3.0e-8)
    ("Muon magnetic momentto nuclear magneton ratio" -8.8905977027 "" 3.0e-8)
    ("Muon magnetic moment anomaly" 1.1659160264e-3 "" 5.5e-7)
    ("Muon g-factor" -2.002331832013 "" 6.4e-10)
    ("Muon-proton magnetic moment ratio" -3.1833453910 "" 3.2e-8)

    ;; Atomic and Nuclear, Tau
    ("Tau mass" 3.1678852e-27 "kg" 1.6e-4)
    ("Tau mass energy equivalent" 2.8471546e-10 "J" 1.6e-4)
    ("Tau mass energy equivalent" 1777.0529 "MeV" 1.6e-4)
    ("Tau-electron mass ratio" 3477.6057 "" 1.6e-4)
    ("Tau-muon mass ratio" 16.818827 "" 1.6e-4)
    ("Tau-proton mass ratio" 1.8939631 "" 1.6e-4)
    ("Tau-neutron mass ratio" 1.8913531 "" 1.6e-4)
    ("Tau molar mass" 1.9077431e-3 "kg mol-1" 1.6e-4)
    ("Tau Compton wavelength" 0.6977011e-15 "m" 1.6e-4)
    
    ;; Atomic and Nuclear, Proton
    ("Proton mass" 1.6726215813E-27 "kg" 7.9e-8)
    ("Proton mass energy equivalent" 1.5032773112E-10 "J" 7.9e-8)
    ("Proton mass energy equivalent" 938.27199838 "MeV" 4.0e-8)
    ("Proton-electron mass ratio" 1836.152667539 "" 2.1e-9)
    ("Proton-muon mass ratio" 8.8802440827 "" 3.0e-8)
    ("Proton-tau mass ratio" 0.52799486 "" 1.6e-4)
    ("Proton-neutron mass ratio" 0.9986234785558 "" 5.8e-10)
    ("Proton charge to mass quotient" 9.5788340838e7 "C kg-1" 4.0e-8)
    ("Proton molar mass" 1.0072764668813e-3 "kg mol-1" 1.3e-10)
    ("Proton Compton wavelength" 1.32140984710e-15 "m" 7.6e-9)
    ("Proton magnetic moment" 1.41060663358e-26 "J T-1" 4.1e-8)
    ("Proton magnetic moment to Bohr magneton ratio" 1.52103220315e-3 "" 1.0e-8)
    ("Proton magnetic moment to nuclear magneton ratio" 2.79284733729 "" 1.0e-8)
    ("Proton g-factor" 5.58569467557 "" 1.0E-8)
    ("Proton-neutron magnetic moment ratio" -1.4598980534 "" 2.4e-7)
    ("Shielded proton magnetic moment" 1.41057039959e-26 "J T-1" 4.2e-8)
    ("Shielded proton magnetic moment-Bohr magneton ratio"  1.52099313216e-3 "" 1.1e-8)
    ("Shielded proton magnetic moment-nuclear magneton ratio" 2.79277559731 "" 1.1e-8)
    ("Proton magnetic shielding correction" 25.68715e-6 "" 5.7e-4)
    ("Proton gyromagnetic ratio" 2.6752221211e8 "s-1 T-1" 4.1e-8)
    ("Proton gyromagnetic ratio" 42.577482518 "MHz T-1" 4.1e-8)
    ("Shielded proton gyromagnetic ratio" 2.6751534111e8 "s-1 T-1" 4.2e-8)
    
    ;; Atomic and Nuclear, Neutron
    ("Neutron mass" 1.6749271613e-27 "kg" 7.9e-8)
    ("Neutron mass energy equivalent" 1.505349461e-10 "J" 7.9e-8)
    ("Neutron mass energy equivalent" 939.56533038 "MeV" 4.0e-8)
    ("Neutron-electron mass ratio" 1838.683655040 "" 2.2e-9)
    ("Neutron-muon mass ratio" 8.8924847827 "" 3.0e-8)
    ("Neutron-tau mass ratio" 0.52872286 "" 1.6e-4)
    ("Neutron-proton mass ratio" 1.0013784188758 "" 5.8E-10)
    ("Neutron molar mass" 1.0086649157855e-3 "kg mol-1" 5.4e-10)
    ("Neutron Compton wavelength" 1.31959089810e-15 "m" 7.6e-9)
    ("Neutron magnetic moment" -0.9662364023e-26 "J T-1" 2.4e-7)
    ("Neutron magnetic moment to Bohr magneton ratio" -1.041875632e-3 "" 2.4e-7)
    ("Neutron magnetic moment to nuclear magneton ratio" -1.9130427245 "" 2.4e-7)
    ("Neutron g-factor" -3.8260854590 "" 2.4e-7)
    ("Neutron-electron magnetic moment ratio" 1.0406688225e-3 "" 2.4e-7)
    ("Neutron-proton magnetic moment ratio" -0.6849793416 "" 2.4e-7)
    ("Neutron to shielded proton magnetic moment ratio" -0.6849969416 "" 2.4e-7)
    ("Neutron gyromagnetic ratio" 1.8324718844e8 "s-1 T-1" 2.4e-7)
    
    ;; Atomic and Nuclear, Deuteron
    ("Deuteron mass" 3.3435830926e-27 "kg" 7.9e-8)
    ("Deuteron mass energy equivalent" 3.0050626224e-10 "J" 7.9e-8)
    ("Deuteron mass energy equivalent" 1875.61276275 "MeV" 4.0e-8)
    ("Deuteron-electron mass ratio" 3670.482955078 "" 2.1e-9)
    ("Deuteron-proton mass ratio" 1.9990075008341 "" 2.0e-10)
    ("Deuteron molar mass" 2.0135532127135e-3 "kg mol-1" 1.7e-10)
    ("Deuteron magnetic moment" 0.43307345718e-26 "J T-1" 4.2E-8)
    ("Deuteron magnetic moment to Bohr magneton ratio" 0.466975455650e-3 "" 1.1e-8)
    ("Deuteron magnetic moment to nuclear magneton ratio" 0.857438228494 "" 1.1e-8)
    ("Deuteron-electron magnetic moment ratio" -4.66434553750e-4 "" 1.1e-8)
    ("Deuteron-proton magnetic moment ratio" 0.307012208345 "" 1.5e-8)
    ("Deuteron-neutron magnetic moment ratio" -0.4482065211 "" 2.4e-7)

    ;; Atomic and Nuclear, Helion
    ("Helion mass" 5.0064117439E-27 "kg" 7.9e-8)
    ("Helion mass energy equivalent" 4.499538483e-10 "J" 7.9e-8)
    ("Helion mass energy equivalent" 2808.3913211 "MeV" 4.0e-8)
    ("Helion-electron mass ratio" 5495.88523812 "" 2.1e-9)
    ("Helion-proton mass ratio" 2.9931526585093 "" 3.1e-10)
    ("Helion molar mass" 3.0149322346986e-3 "kg mol-1" 2.8e-10)
    ("Shielded Helion magnetic moment" -1.07455296745E-26 "J T-1" 4.2e-8)
    ("Shielded Helion magnetic moment to Bohr magneton ratio" -1.15867147414e-3 "" 1.2e-8)
    ("Shielded Helion magnetic moment to nuclear magneton ratio" -2.12749771825 "" 1.2e-8)
    ("Shielded helion to proton magnetic moment ratio" -0.76176656312 "" 1.5e-8)
    ("Shielded helion to shielded proton magnetic moment ratio" -0.761786131333 "" 4.3e-9)
    ("Shielded helion gyromagnetic ratio" 2.03789476485e8 "s-1 T-1" 4.2e-8)
    
    ;; Atomic and Nuclear, Alpha particle
    ("Alpha particle mass" 6.6446559852e-27 "kg" 7.9e-8)
    ("Alpha particle mass energy equivalent" 5.9719189747e-10 "J" 7.9e-8)
    ("Alpha particle mass energy equivalent" 3727.3790415 "MeV" 4.0e-8)
    ("Alpha particle to electron mass ratio" 7294.29950816 "" 2.1e-9)
    ("Alpha particle to proton mass ratio" 3.972599684611 "" 2.8e-10)
    ("Alpha particle molar mass" 4.001506174710e-3 "kg mol-1" 2.5e-10)
    
    ;; Physico-Chemical
    ("Avogadro constant" 6.0221419947e23 "mol-1" 7.9e-8)
    ("Atomic mass constant" 1.6605387313e-27 "kg" 7.9e-8)
    ("Atomic mass constant energy equivalent" 1.4924177812e-10 "J" 7.9e-8)
    ("Atomic mass constant energy equivalent" 931.49401337 "MeV" 4.0e-8)
    ("Faraday constant" 96485.341539 "C mol-1" 4.0e-8)
    ("Molar Planck constant" 3.99031268930e-10 "J s mol-1" 7.6e-9)
    ("Molar Planck constant" 0.1196265649291 "J m mol-1" 7.6e-9)
    ("Molar gas constant" 8.31447215 "J mol-1 K-1" 1.7e-6)
    ("Molar volume of ideal gas (T=273.15K, P=101.325 kPa)" 22.41399639e-3 "m3 mol-1" 1.7e-6)
    ("Sackur-Tetrode constant (T=1K, P=100kPa)" -1.1517048444 "" 3.8e-6)
    ("Sackur-Tetrode constant (T=1K, P=101.325kPa)" -1.164867844 "" 3.7e-6)
    ("Stefan-Boltzmann constant" 5.67040040e-8 "W m-2 K-4" 7.0e-6)
    ("First radiation constant" 3.7417710729e-16 "W m2" 7.8e-8)
    ("First radiation constant for spectral radiance" 1.19104272293e-16 "W m2 sr-1" 7.8e-8)
    ("second radiation constant" 1.438775225e-2 "m K" 1.7e-6)
    ("Wien displacement law constant" 2.897768651e-3 "m K" 1.7e-6)))

;; This operation throws away a lot of the data in the previous list of constants. It
;; drops the relative error, and incorporates the units (if present) into the constant
;; name. This is done to simplify the use of this list later on, since none of the other
;; values are currently used.
(eval-when (:load-toplevel)
  (set! *constant-library*
        (map (lambda (entry)
               (cons (if (scheme::= (string-length (list-ref entry 2)) 0)
                         (list-ref entry 0)
                         (string-append (list-ref entry 0)
                                        " ("
                                        (list-ref entry 2)
                                        ")"))
                     (list-ref entry 1)))
             *constant-library*)))

