// C lang code parsing program to eliminate defines and other elements unwanted by code-checker | //! fold all suggested
/* #region MACROS */
/* --- LIBRARIES --- */
#include <bits/stdc++.h>
using namespace std;

/* --- SUPERFOR --- */
#define GET_MACRO_FOR(_1, _2, _3, _4, NAME, ...) NAME
#define _FOR3(i, a, n, inc) for (auto i = (a); (inc) > 0 ? i < (n) : i >= (n); i += (inc))
#define _FOR2(i, a, n) _FOR3(i, a, n, 1)
#define _FOR1(i, n) _FOR2(i, 0, n)
#define _FOR0(n) _FOR1(i, n)
#define FOR(...) GET_MACRO_FOR(__VA_ARGS__, _FOR3, _FOR2, _FOR1, _FOR0)(__VA_ARGS__)    //? (/name/, //from//, to, ///inc///)

/* --- MISC --- */
#define cerr if (debug_mode) cout
/* #endregion */

/* #region //! --- CONSTS --- */
const bool debug_mode = true;
const string program_filename = "code_parser.exe";

const string headline_includes = "Patryk Flama";        // that is what headline SHOULD include (to detect if present)
      string headline = "// Patryk Flama ";         // headline formula
const string libraries[] = {"#include <stdio.h>", "#include <stdbool.h>", "#include <string.h>", "#include <stdlib.h>", "#include <ctype.h>", "#include <stddef.h>", "#include <float.h>", "#include <math.h>"};   // all c libraries to include 
const string macros_location = "template.c";

ifstream input_file;
ofstream output_file;
/* #endregion */

/* #region //! ---FUNCTIONS DECLARATION--- */
string get_file_name(string relative_path, string file_extension);
string get_file_path(string program_path, string relative_file_path);
string get_file_folder_name(string relative_path);

void make_headline();
void remove_macros();
void replace_macros();

string replace_FORs(string line);
string replace_minmax(string line, string replace);
/* #endregion */


//! ---------- MAIN ----------
int main(int argc, char** argv){    // argv = {exec_file_name, file_to_parse_name, (output_file_name)}
    if(argc == 1){
        cout << "No file name! How to use: ./" << program_filename << " [relative path to file] (output filename - optional)\n";
        return 0;
    }

    /* #region //? --- setting names, constants, general setup ---*/
    string file_extension = ".c";
    string file_name = get_file_name(argv[1], file_extension);                  // name of the processed file, without extension
    string file_path = get_file_path(argv[0], argv[1]);         // full path to the processed file
    string file_folder_name = get_file_folder_name(argv[1]);    // name of the last folder before the processed file - should be list number
    // string output_file_name = file_name+"_parsed"+file_extension;              // set output file name
    string output_file_name = "program_parsed"+file_extension;
    headline += file_folder_name + ' ' + file_name;
    if(argc == 3) output_file_name = argv[2];

    input_file.open(argv[1]);
    if(!input_file){
        cout << "Input file couldn't be opened!\n";
        return 0;
    }
    output_file.open(output_file_name);
    if(!output_file){
        cout << "Output file couldn't be opened!\n";
        return 0;
    }
    /* #endregion */

    /* #region //? --- parsing file ---*/
    make_headline();
    remove_macros();
    replace_macros();
    /* #endregion */

    cout << "Success!\n";

    input_file.close();
    output_file.close();
}


/* #region //! --- FUNC ---- */
//? --- getting names and constants ---
string get_file_name(string relative_path, string file_extension){     // cut subdirectories of relative file path (leave just filename)
    int ptr = relative_path.size()-1;
    while(ptr >= 0 && (relative_path[ptr] != '/' && relative_path[ptr] != '\\')) ptr--;
    
    return relative_path.substr(ptr+1, relative_path.size()-ptr-file_extension.size()-1);
}

string get_file_path(string program_path, string relative_file_path){       // get complete file path
    while(program_path.size() > 0 && (program_path[program_path.size()-1] != '/' && program_path[program_path.size()-1] != '\\')){
        program_path.pop_back();
    }
    return program_path + relative_file_path;
}

string get_file_folder_name(string relative_path){      // get name of folder above file
    int ptr_after; int ptr_before = ptr_after = relative_path.size();  // .../(<- ptr before) |foldername| (ptr after ->)/filename
    while(ptr_after >= 0 && (relative_path[ptr_after] != '/' && relative_path[ptr_after] != '\\')) ptr_after--;
    ptr_before = ptr_after-1;
    while(ptr_before >= 0 && (relative_path[ptr_before] != '/' && relative_path[ptr_before] != '\\')) ptr_before--;

    if(ptr_after < 0) return "";
    return relative_path.substr(ptr_before+1, ptr_after-ptr_before-1);
}

//? --- parsing code ---
void make_headline(){
    string line;
    streampos old_file_pos = input_file.tellg();
    getline(input_file, line);

    if(line.find(headline_includes) == string::npos){
        input_file.seekg(old_file_pos);
        output_file << headline << "\n\n";
    } else{
        output_file << line << '\n';
    }
}

void remove_macros(){      // removes (first region section - macros section) or (macros include formula)
    string line;
    while(getline(input_file, line)){
        if(line.find("#region") != string::npos){                           // if beggining of macros region
            while(getline(input_file, line) && line.find("#endregion") == string::npos){    // till the end of file or macros region
                if(line.find("#include") != string::npos) output_file << line << '\n';
            }
            return;
        } else if(line.find(macros_location) != string::npos){              // if macros are beeing included
            for(auto &s:libraries) output_file << s << '\n';
            return;
        }

        output_file << line << '\n';
    }
}

void replace_macros(){
    string line;
    while(getline(input_file, line)){
        output_file << replace_minmax(replace_minmax(replace_FORs(line), "min"), "max") << '\n';
    }
}

//? --- replacing macros ---

string replace_FORs(string line){
    int location_FOR = line.find("FOR");
    if(location_FOR == string::npos) return line;

    const int max_args_FOR = 4;
    string variable_type_FOR = "auto";
    string output_string = "";

    int ptr = 0;
    while(ptr < line.size()){
        while(ptr < location_FOR) output_string += line[ptr++];      // dont change file till FOR occurence
        int stage_FOR = 0;
        string args_FOR[max_args_FOR];        // (/name/, //from//, to, ///inc///)
        
        while(line[ptr] != '(') ptr++;  // go to FOR values
        int nesting = 1;
        ++ptr;
        while(nesting > 0){
            if(line[ptr] == ',') stage_FOR++;
            else if(line[ptr] != ' ') args_FOR[stage_FOR] += line[ptr];

            ++ptr;
            if(line[ptr] == '(') nesting++;
            if(line[ptr] == ')') nesting--;
        } ptr++;
        
        switch (stage_FOR){     // build fors from FORs
        case 0:
            output_string += "for(" + variable_type_FOR + " i = 0; i < " + args_FOR[0] + "; i++)";
            break;
        case 1:
            output_string += "for(" + variable_type_FOR + ' ' + args_FOR[0] + " = 0; " + args_FOR[0] + " < " + args_FOR[1] + "; " + args_FOR[0] + "++)";
            break;
        case 2:
            output_string += "for(" + variable_type_FOR + ' ' + args_FOR[0] + " = " + args_FOR[1] + "; " + args_FOR[0] + " < " + args_FOR[2] + "; " + args_FOR[0] + "++)";
            break;
        case 3:
            output_string += "for(" + variable_type_FOR + ' ' + args_FOR[0] + " = " + args_FOR[1] + "; " + args_FOR[0] + " < " + args_FOR[2] + "; " + args_FOR[3] + ")";
            break;
        }

        location_FOR = line.find("FOR", ptr);
        if(location_FOR == string::npos) while(ptr < line.size()) output_string += line[ptr++];
    }

    return output_string;
}

string replace_minmax(string line, string replace){
    int location_minmax = line.find(replace+'(');
    if(location_minmax == string::npos) return line;
    
    string output_string = "";
    int ptr = 0;
    while(ptr < line.size()){
        while(ptr < location_minmax) output_string += line[ptr++];
        string a, b; // two formulas to compare

        while(line[ptr] != '(') ptr++;
        int nesting = 1;
        ++ptr;
        while(nesting > 0){
            if(line[ptr] == ',') swap(b, a);
            else if(line[ptr] != ' ') b += line[ptr];

            ++ptr;
            if(line[ptr] == '(') nesting++;
            if(line[ptr] == ')') nesting--;
        } ptr++;

        output_string += '(' + a + (replace == "min" ? " < " : " > ") + b + " ? " + a + " : " + b + ')';

        location_minmax = line.find(replace+'(', ptr);
        if(location_minmax == string::npos) while(ptr < line.size()) output_string += line[ptr++];
    }

    return output_string;
}

/* #endregion */

/* #region CODE IDEA */ /*
file must:
* [x] include headline if not present: //* Patryk Flama $(FolderName) $(filename) 
* [x] if link to _bettercode_ is present:
    * [x] add libraries
* [x] else:
    * [x] leave libraries in place
    * [x] delete first #region - which should be there

* [x] change all SUPERFOR'S to normal for's
* [x] replace other shortcuts: fill
/* #endregion */
