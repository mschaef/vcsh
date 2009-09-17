// TextServGUIDs.cpp

#include "stdafx.h"

// These IIDs are declared in TextServ.h and defined in RichEd20.lib,
// which we link with. So it should not be necessary to define them here.
// However, for some inscrutable microsoftian reason the definitions in
// RichEd20.lib have different values, that do not work! (after
// CreateTextServices, QueryInterface for the IID_ITextServices defined
// in RichEd20.lib fails!)
//
// The following apparently correct values come from the "windowlessRE"
// sample. Hopefully this will no longer be needed with a more recent
// platform SDK (?)

EXTERN_C const IID IID_ITextServices = { // 8d33f740-cf58-11ce-a89d-00aa006cadc5
    0x8d33f740, 0xcf58, 0x11ce, { 0xa8, 0x9d, 0x00, 0xaa, 0x00, 0x6c, 0xad, 0xc5 }
};

EXTERN_C const IID IID_ITextHost = { // c5bdd8d0-d26e-11ce-a89e-00aa006cadc5
    0xc5bdd8d0, 0xd26e, 0x11ce, { 0xa8, 0x9e, 0x00, 0xaa, 0x00, 0x6c, 0xad, 0xc5 }
};

EXTERN_C const IID IID_ITextDocument = {
	0x8CC497C0, 0xA1DF, 0x11CE, { 0x80, 0x98, 0x00, 0xAA, 0x00, 0x47, 0xBE, 0x5D }
};

