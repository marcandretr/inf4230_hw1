/* INF4230 - Intelligence artificielle
 * UQAM / Département d'informatique
 * Automne 2014 / TP1 - Algorithme A*
 * http://ericbeaudry.ca/INF4230/tp1/
 */

#include <string>
#include <iostream>
#include <fstream>
#include <vector>

using namespace std;

struct Position{
    Position(int l=0, int c=0) : ligne(0), colonne(0){}
    int ligne, colonne;
};

class Grille{
    private:
      vector<string> lignes;
      vector<Position> buts;
      Position positionBonhomme;

    public:
      bool deplacer(char direction);
      bool butSatisfait() const;
      char operator[](const Position& p) const;
      char& operator[](const Position& p);
    
    friend istream& operator >> (istream&, Grille& g);
    friend ostream& operator << (ostream&, const Grille& g);
};

istream& operator >> (istream& is, Grille& g){
    g.lignes.clear();
    g.buts.clear();
    Position p;
    while(is && !is.fail())
    {
        string ligne;
        getline(is, ligne);
        if(ligne.empty())
            break;
        for(p.colonne=0;p.colonne<ligne.size();p.colonne++)
            switch(ligne[p.colonne]){
                case '*':
                case '+':
                    ligne[p.colonne]=' ';
                case ' ':
                case '#':
                case '$':
                    break;
                case '@':
                    g.positionBonhomme = p;
                    ligne[p.colonne]=' ';
                    break;
                case '.':
                    ligne[p.colonne]=' ';
                    g.buts.push_back(p);
                    break;
            }
        g.lignes.push_back(ligne);
        p.ligne++;
    }
    return is;
}

ostream& operator << (ostream& os, const Grille& g){
    for(int l=0;l<g.lignes.size();l++)
        os << g.lignes[l] << endl;
    return os;
}

char Grille::operator[](const Position& p) const{
    return lignes[p.ligne][p.colonne];
}
char& Grille::operator[](const Position& p){
    return lignes[p.ligne][p.colonne];
}

bool Grille::deplacer(char direction){
    Position p2 = positionBonhomme;
    Position p3 = positionBonhomme;
    switch(direction){
        case 'N':
            p2.ligne--; p3.ligne-=2;
            break;
        case 'S':
            p2.ligne++; p3.ligne+=2;
            break;
        case 'W':
            p2.colonne--; p3.colonne-=2;
            break;
        case 'E':
            p2.colonne++; p3.colonne+=2;
            break;
        default:
            return false;
    }
    if(operator[](p2)=='#') return false;
    if(operator[](p2)=='$'){
        if(operator[](p3)!=' ') return false;
        operator[](p3) = '$';
    }
    operator[](positionBonhomme) = ' ';
    operator[](p2) = '@';
    positionBonhomme = p2;
    return true;
}

bool Grille::butSatisfait() const{
    for(int b=0;b<buts.size();b++)
        if(operator[](buts[b])!='$')
            return false;
    return true;
}

vector<char> chargerPlan(istream& in){
    vector<char> plan;
    while(in && !in.fail()){
        if(in.peek()=='#'){
            string ligne;
            getline(in, ligne);
        }else{
            char c;
            in >> c >> std::ws;
            if(!in.fail())
                plan.push_back(c);
        }
    }
    return plan;
}

int main(int argc, const char** argv)
{
    if(argc<3){
        cout << "./valideur-sokoban grille.txt resultat [solution]" << endl;
        return 1;
    }
    Grille g;
    {
        ifstream ingrille(argv[1]);
        if(ingrille.fail()) return 2;
        ingrille >> g;
    }
    vector<char> resultat, solution;
    int longueurSolution=-1;
    {
        ifstream inresultat(argv[2]);
        if(inresultat.fail()) return 3;
        resultat = chargerPlan(inresultat);
        //cout << "|resultat|=" << resultat.size() << endl;
    }
    if(argc>3){
        ifstream insolution(argv[3]);
        if(!insolution.fail()){
            solution = chargerPlan(insolution);
            longueurSolution = solution.size();
        }
        //cout << "|solution|=" << longueurSolution << endl;
    }
    
    bool resultatcorrect = true;
    //cout << "Grille lu:\n" << g << endl;
    for(int i=0;i<resultat.size();i++){
        //cout << "Move: " << resultat[i] << endl;
        if(!g.deplacer(resultat[i])){
            resultatcorrect = false;
            cout << "ÉchecMove" << endl;
            break;
        }
        //cout << g << endl;
    }
    resultatcorrect &= g.butSatisfait();
    
    if(resultatcorrect){
        if(longueurSolution==-1)
            cout << "AuMoinsCorrect";
        else
            if(resultat.size()<longueurSolution) cout << "Meilleur";
            else if(resultat.size()==longueurSolution) cout << "Optimal";
            else cout << "CorrectNonOptimal";
    }else
        cout << "Echec";
    cout << endl;
    
    return 0;
}
