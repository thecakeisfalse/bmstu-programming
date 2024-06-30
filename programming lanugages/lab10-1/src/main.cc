#include <bits/stdc++.h>

using namespace std;

class WordString {
  private:
    vector<string> words;
    string s;

  public:
    WordString(string s) : s(s) {
        stringstream ss(s);
        for (string w; ss >> w;)
            words.push_back(w);
    }

    auto begin() { return words.begin(); }
    auto end() { return words.end(); }

    auto operator[](int i) const {
        assert(i < s.length());
        return s[i];
    }

    auto size() const { return s.size(); }
};

int main() {
    // WordString s("Hello    world    wyring    ksdjfkd sd\nsdjfksdk 12skjsdksdkfjsdkf s\td");

    string ss = "Hel lo wo rld";
    WordString s = ss;

    for (auto st = s.begin(); st != s.end(); ++st)
        cout << (*st) << '\n';

    // for (auto &x : s)
    //     cout << x << '\n';

    for (int i = 0; i < s.size(); ++i)
        cout << s[i] << ' ';
    cout << '\n';
}
