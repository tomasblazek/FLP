# simplify-bkg
Implementace algoritmu, který odstraňuje zbytečné symboly z bezkotextové gramatiky generující neprázdný jazyk.

## Použití
Program se jmenuje simplify-bkg a je možné jej spustit takto: `simplify-bkg volby [vstup]`
- **vstup** je jméno vstupního souboru (pokud není specifikováno, program čte standartní vstup) obsahující BKG ve formátu specifikovanou v zadání projektu
- **volby** jsou parametry ovlivňující chování programu a může být zadána právě jedna volba:
    - *-i* - vypíše se pouze načtená a zvalidovaná gramatika, která je uložená ve vnitřní logice programu
    - *-1* - vypíše se BKG G1 po prvním kroku algoritmu 4.3 z TIN opory
    - *-2* - vypíše se BKG G2 po druhém kroku algoritmu 4.3 z TIN opory

## Dodatečné informace
Terminály, neterminály a pravidla mohou být na výstupu v odlišném pořadí než byly na vstupu. Jsou seřazeny lexikálně.
