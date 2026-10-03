module Plugin

import IO;
import ParseTree;
import util::Reflective;
import util::IDEServices;
import util::LanguageServer;
import Relation;

import Syntax;

PathConfig pcfg = getProjectPathConfig(|project://objectilang|);
Language objectilangLang = language(pcfg, "Objectilang", "obl", "Plugin", "contribs");

set[LanguageService] contribs() = {
    parser(start[Programa] (str program, loc src) {
        return parse(#start[Programa], program, src);
    })
};

void main() {
    registerLanguage(objectilangLang);
}
