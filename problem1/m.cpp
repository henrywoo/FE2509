#include <stdio.h>
#include <random>
#include <cmath>
#include <time.h>
#include <vector>

using namespace std;

const int ITERNUM = 1e7;

void firstSimulation(){
  time_t t1 = time(NULL);

  std::random_device rd;
  std::mt19937 gen(rd());

  std::normal_distribution<> d(.05 / 253, .23 / sqrt(253));
  double initialprincipal = 1e6;
  int j = 0;
  for (int i = 0; i < ITERNUM; ++i){
    double tmp = initialprincipal;
    for (int n = 0; n < 45; ++n) {
      tmp *= exp(d(gen));
      if (tmp < initialprincipal*0.95){
        j++;
        break;
      }
    }
    /*if (i % 1000001 == 10000){
      printf("%.5f\t", (1.0*j) / i);
    }*/
  }
  time_t t2 = time(NULL);
  printf("%.5f\ntime:%ld\n", (1.0*j) / ITERNUM, (t2 - t1));
}

void secondSimulation(){
  double initialprincipal = 1e6;

  vector<double*> vvd;
  std::random_device rd;
  std::mt19937 gen(rd());
  std::normal_distribution<> d(.05 / 253, .23 / sqrt(253));
  for (int x = 0; x < 45;++x){
    vvd.push_back(new double[ITERNUM]);
    for (int i = 0; i < ITERNUM; ++i){
      *(vvd[x]+i)=d(gen);
    }
  }

  int k = 0;
  for (int i = 0; i < ITERNUM; ++i){
    double tmp = initialprincipal;
    for (int j = 0; j < 45; ++j){
      tmp *= exp(*(vvd[j]+i));
      if (tmp < initialprincipal*0.95){
        k++;
        break;
      }
    }
  }
  printf("%.5f\n", (1.0*k) / ITERNUM);
}

int main(int argc, char* argv[]){
  firstSimulation();
  secondSimulation();
  return 0;
}