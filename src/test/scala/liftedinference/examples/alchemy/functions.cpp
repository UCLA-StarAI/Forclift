#include <iostream>
#include <string>
#include <sstream>
using namespace std;

// helper functions for int/string conversion
bool stringToInt(const string &s, int &i);

string intToString(int i);

#ifdef __cplusplus
extern "C" {
#endif

string max(string a, string b)
{
  int i,j;
  if (stringToInt(a,i) && stringToInt(b,j))
    if (i >= j)
  	  return intToString(i);
  	else
  	  return intToString(j);
  else
  {
  	cout << "In function max: the constant " << a << " or "
  		 << b << " does not appear to be an integer." << endl;
  	exit(-1);
  }
}

string min(string a, string b)
{
  int i,j;
  if (stringToInt(a,i) && stringToInt(b,j))
    if (i < j)
  	  return intToString(i);
  	else
  	  return intToString(j);
  else
  {
  	cout << "In function min: the constant " << a << " or "
  		 << b << " does not appear to be an integer." << endl;
  	exit(-1);
  }
}

#ifdef __cplusplus
}
#endif

bool stringToInt(const string &s, int &i)
{
  istringstream iss(s);
  
  if (iss>>i)
    return true;
  else
    return false;
}

string intToString(int i)
{
  ostringstream oss;
  oss << i << flush;  
  return(oss.str());
}
