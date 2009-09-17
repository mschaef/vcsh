// bstream_test.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include "util_stream.h"
#include "util_crypt.h"
#include <malloc.h>

char *plainText = \
	"Along with Hamlet, King Lear, and Macbeth, Othello is one of Shakespeare's four"
	"great tragedies and thus a pillar of what most critics take to be the apex of"
	"Shakespeare's dramatic art. More than anything else, what distinguishes Othello"
	"from its great tragedies peers is the role of its villain, Iago. While the usurper"
	"King Claudius of Hamlet, the faithless daughters of Lear, and the unnatural"
	"villains of Macbeth (Macbeth, his Lady and the Weird Sister witches) are all"
	"impressively evil in their own way, none of them enjoys the same diabolical role as"
	"Iago. Iago is a character who essentially writes the play's main plot, takes a key"
	"part in it, and gives first-hand direction to the others, most notably to the noble"
	"Moor, Othello. The play presents us with two remarkable characters, Iago and his"
	"victim, with Iago as the dominant force which causes Othello to see the infidelity"
	"of his young and beautiful wife, Desdemona, with his favorite lieutenant, Michael"
	"Cassio. Indeed, not only is \"seeing\" and the gap between appearance and reality a"
	"central theme of the play, it overlaps with other major thematic strands (trust, honor,"
	"and reputation) and sheds light on still others, including the theme of patriarchy and"
	"the political state.";



int _tmain(int argc, _TCHAR* argv[])
{
	/*
	u32 test[4] = { 0xCAFEBABE,
				    0xDEADBEEF,
	                0xFEEDCAFE,
					0xBEEFFEED };
					*/
	u32 test[4] = { 0x00000000,
				    0xFFFFFFFF,
	                0x55555555,
					0x12481248 };

	u8 unpacked[sizeof(test) * BITS_PER_BYTE];


	unpack_bytes(unpacked, (u8 *)test, sizeof(test));

	pack_bytes((u8 *)test, unpacked, sizeof(test));

	return 0;
}

