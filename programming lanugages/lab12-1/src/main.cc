#include <bits/stdc++.h>
#include <sys/dir.h>

using namespace std;

using stringSet = unordered_multiset<string>;

string getFileExtension(string path) { return path.substr(path.find_last_of('.') + 1); }

stringSet getWordsFromFile(string path) {
    stringSet result;
    ifstream input(path);

    assert(input.good());

    for (string w; input >> w;)
        result.insert(w);

    return result;
}

vector<string> getAllFilesInDirectory(string path, string extension) {
    vector<string> result;
    DIR *wd = opendir(path.c_str());

    if (wd == NULL) {
        perror("opendir");
        return result;
    }

    for (struct dirent *ent; (ent = readdir(wd)) != NULL;) {
        if (ent->d_type == DT_REG) {
            string filename = path + "/" + ent->d_name;
            if (getFileExtension(filename) == extension) {
                result.push_back(filename);
            }
        }
    }

    return result;
}

vector<string> getAllWords(string path, string extension) {
    stringSet all;

    for (const auto &x : getAllFilesInDirectory(path, extension)) {
        auto words = getWordsFromFile(x);
        all.merge(words);
    }

    vector<string> all_v(all.begin(), all.end());
    sort(all_v.begin(), all_v.end());
    return all_v;
}

vector<string> getSharedWords(string path, string extension) {
    stringSet shared;
    bool share = true;

    for (const auto &x : getAllFilesInDirectory(path, extension)) {
        auto words = getWordsFromFile(x);
        if (share) {
            for (const auto &x : words) {
                shared.insert(x);
            }
            share = false;
        } else {
            stringSet temp;
            for (const auto &x : words) {
                if (shared.find(x) != shared.end()) {
                    shared.erase(x);
                    temp.insert(x);
                }
            }
            shared = temp;
        }
    }

    vector<string> shared_v(shared.begin(), shared.end());
    sort(shared_v.begin(), shared_v.end());
    return shared_v;
}

void saveWordsInFile(vector<string> v, string s) {
    ofstream output(s);
    assert(output.good());

    for (const auto &x : v)
        output << x << '\n';
}

int main() {
    string path = "test directory";

    bool share = true;

    auto all = getAllWords(path, "txt");
    auto shared = getSharedWords(path, "txt");

    saveWordsInFile(all, "all.txt");
    saveWordsInFile(shared, "shared.txt");
}
