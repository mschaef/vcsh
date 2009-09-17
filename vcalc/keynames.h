/* This is a table of key information. It's how we map from keypress
 * events into descriptively named symbols.  It also contains a field
 * that indicates if the WM_KEYDOWN event for that virtual key should
 * normally produce a WM_CHAR message.
 **/

struct keyInfo {
	char *name;
	bool encodes_char;
};

keyInfo keyinfoTable[] = {
   { "key-0x00",                    FALSE },
   { "key-lbutton",                 FALSE }, // 0x01
   { "key-rbutton",                 FALSE }, // 0x02
   { "key-cancel",                  FALSE }, // 0x03
   { "key-mbutton",                 FALSE }, // 0x04
   { "key-xbutton1",                FALSE }, // 0x05
   { "key-xbutton2",                FALSE }, // 0x06
   { "key-0x07",                    FALSE },
   { "key-back",                    FALSE }, // 0x08
   { "key-tab",                     FALSE }, // 0x09
   { "key-0x0a",                    FALSE },
   { "key-0x0b",                    FALSE },
   { "key-clear",                   FALSE }, // 0x0C
   { "key-return",                  FALSE }, // 0x0D
   { "key-0x0e",                    FALSE },
   { "key-0x0f",                    FALSE },
   { "key-shift",                   FALSE }, // 0x10
   { "key-control",                 FALSE }, // 0x11
   { "key-menu",                    FALSE }, // 0x12
   { "key-pause",                   FALSE }, // 0x13
   { "key-capital",                 FALSE }, // 0x14
   { "key-kana",                    FALSE }, // 0x15
   { "key-0x16",                    FALSE },
   { "key-junja",                   FALSE }, // 0x17
   { "key-final",                   FALSE }, // 0x18
   { "key-kanji",                   FALSE }, // 0x19
   { "key-0x1a",                    FALSE },
   { "key-escape",                  FALSE }, // 0x1B
   { "key-convert",                 FALSE }, // 0x1C
   { "key-nonconvert",              FALSE }, // 0x1D
   { "key-accept",                  FALSE }, // 0x1E
   { "key-modechange",              FALSE }, // 0x1f
   { "key-space",                   FALSE }, // 0x20
   { "key-prior",                   FALSE }, // 0x21
   { "key-next",                    FALSE }, // 0x22
   { "key-end",                     FALSE }, // 0x23
   { "key-home",                    FALSE }, // 0x24
   { "key-left",                    FALSE }, // 0x25
   { "key-up",                      FALSE }, // 0x26
   { "key-right",                   FALSE }, // 0x27
   { "key-down",                    FALSE }, // 0x28
   { "key-select",                  FALSE }, // 0x29
   { "key-print",                   FALSE }, // 0x2A
   { "key-execute",                 FALSE }, // 0x2B
   { "key-snapshot",                FALSE }, // 0x2C
   { "key-insert",                  FALSE }, // 0x2D
   { "key-delete",                  FALSE }, // 0x2E
   { "key-help",                    FALSE }, // 0x2F
   { "key-0",                       TRUE  }, // 0x30
   { "key-1",                       TRUE  }, // 0x31
   { "key-2",                       TRUE  }, // 0x32
   { "key-3",                       TRUE  }, // 0x33
   { "key-4",                       TRUE  }, // 0x34
   { "key-5",                       TRUE  }, // 0x35
   { "key-6",                       TRUE  }, // 0x36
   { "key-7",                       TRUE  }, // 0x37
   { "key-8",                       TRUE  }, // 0x38
   { "key-9",                       TRUE  }, // 0x39
   { "key-0x3a",                    FALSE },
   { "key-0x3b",                    FALSE },
   { "key-0x3c",                    FALSE },
   { "key-0x3d",                    FALSE },
   { "key-0x3e",                    FALSE },
   { "key-0x3f",                    FALSE },
   { "key-0x40",                    FALSE },
   { "key-a",                       TRUE  }, // 0x41	
   { "key-b",                       TRUE  },
   { "key-c",                       TRUE  },
   { "key-d",                       TRUE  },
   { "key-e",                       TRUE  },
   { "key-f",                       TRUE  },
   { "key-g",                       TRUE  },
   { "key-h",                       TRUE  },
   { "key-i",                       TRUE  },
   { "key-j",                       TRUE  },
   { "key-k",                       TRUE  },
   { "key-l",                       TRUE  },
   { "key-m",                       TRUE  },
   { "key-n",                       TRUE  },
   { "key-o",                       TRUE  },
   { "key-p",                       TRUE  },
   { "key-q",                       TRUE  },
   { "key-r",                       TRUE  },
   { "key-s",                       TRUE  },
   { "key-t",                       TRUE  },
   { "key-u",                       TRUE  },
   { "key-v",                       TRUE  },
   { "key-w",                       TRUE  },
   { "key-x",                       TRUE  },
   { "key-y",                       TRUE  },
   { "key-z",                       TRUE  },
   { "key-lwin",                    FALSE },
   { "key-rwin",                    FALSE },
   { "key-apps",                    FALSE },
   { "key-0x5e",                    FALSE },
   { "key-sleep",                   FALSE },
   { "key-numpad0",                 TRUE  },
   { "key-numpad1",                 TRUE  },
   { "key-numpad2",                 TRUE  },
   { "key-numpad3",                 TRUE  },
   { "key-numpad4",                 TRUE  },
   { "key-numpad5",                 TRUE  },
   { "key-numpad6",                 TRUE  },
   { "key-numpad7",                 TRUE  },
   { "key-numpad8",                 TRUE  },
   { "key-numpad9",                 TRUE  },
   { "key-multiply",                TRUE  },
   { "key-add",                     TRUE  },
   { "key-separator",               TRUE  },
   { "key-subtract",                TRUE  },
   { "key-decimal",                 TRUE  },
   { "key-divide",                  TRUE  },
   { "key-f1",                      FALSE },
   { "key-f2",                      FALSE },
   { "key-f3",                      FALSE },
   { "key-f4",                      FALSE },
   { "key-f5",                      FALSE },
   { "key-f6",                      FALSE },
   { "key-f7",                      FALSE },
   { "key-f8",                      FALSE },
   { "key-f9",                      FALSE },
   { "key-f10",                     FALSE },
   { "key-f11",                     FALSE },
   { "key-f12",                     FALSE },
   { "key-f13",                     FALSE },
   { "key-f14",                     FALSE },
   { "key-f15",                     FALSE },
   { "key-f16",                     FALSE },
   { "key-f17",                     FALSE },
   { "key-f18",                     FALSE },
   { "key-f19",                     FALSE },
   { "key-f20",                     FALSE },
   { "key-f21",                     FALSE },
   { "key-f22",                     FALSE },
   { "key-f23",                     FALSE },
   { "key-f24",                     FALSE },
   { "key-0x88",                    FALSE },
   { "key-0x89",                    FALSE },
   { "key-0x8a",                    FALSE },
   { "key-0x8b",                    FALSE },
   { "key-0x8c",                    FALSE },
   { "key-0x8d",                    FALSE },
   { "key-0x8e",                    FALSE },
   { "key-0x8f",                    FALSE },
   { "key-numlock",                 FALSE },
   { "key-scroll",                  FALSE },
   { "key-oem-fj-jisho",            FALSE },
   { "key-oem-fj-masshou",          FALSE },
   { "key-oem-fj-touroku",          FALSE },
   { "key-oem-fj-loya",             FALSE },
   { "key-oem-fj-roya",             FALSE },
   { "key-0x97",                    FALSE },
   { "key-0x98",                    FALSE },
   { "key-0x99",                    FALSE },
   { "key-0x9a",                    FALSE },
   { "key-0x9b",                    FALSE },
   { "key-0x9c",                    FALSE },
   { "key-0x9d",                    FALSE },
   { "key-0x9e",                    FALSE },
   { "key-0x9f",                    FALSE },
   { "key-lshift",                  FALSE },
   { "key-rshift",                  FALSE },
   { "key-lcontrol",                FALSE },
   { "key-rcontrol",                FALSE },
   { "key-lmenu",                   FALSE },
   { "key-rmenu",                   FALSE },
   { "key-browser-back",            FALSE },
   { "key-browser-forward",         FALSE },
   { "key-browser-refresh",         FALSE },
   { "key-browser-stop",            FALSE },
   { "key-browser-search",          FALSE },
   { "key-browser-favorites",       FALSE },
   { "key-browser-home",            FALSE },
   { "key-volume-mute",             FALSE },
   { "key-volume-down",             FALSE },
   { "key-volume-up",               FALSE },
   { "key-media-next-track",        FALSE },
   { "key-media-prev-track",        FALSE },
   { "key-media-stop",              FALSE },
   { "key-media-play-pause",        FALSE },
   { "key-launch-mail",             FALSE },
   { "key-launch-media-select",     FALSE },
   { "key-launch-app1",             FALSE },
   { "key-launch-app2",             FALSE },
   { "key-0xb8",                    FALSE },
   { "key-0xb9",                    FALSE },
   { "key-oem-1",                   TRUE  },
   { "key-oem-plus",                TRUE  },
   { "key-oem-comma",               TRUE  },
   { "key-oem-minus",               TRUE  },
   { "key-oem-period",              TRUE  },
   { "key-oem-2",                   TRUE  },
   { "key-oem-3",                   TRUE  },
   { "key-0xc1",                    FALSE },
   { "key-0xc2",                    FALSE },
   { "key-0xc3",                    FALSE },
   { "key-0xc4",                    FALSE },
   { "key-0xc5",                    FALSE },
   { "key-0xc6",                    FALSE },
   { "key-0xc7",                    FALSE },
   { "key-0xc8",                    FALSE },
   { "key-0xc9",                    FALSE },
   { "key-0xca",                    FALSE },
   { "key-0xcb",                    FALSE },
   { "key-0xcc",                    FALSE },
   { "key-0xcd",                    FALSE },
   { "key-0xce",                    FALSE },
   { "key-0xcf",                    FALSE },
   { "key-0xd0",                    FALSE },
   { "key-0xd1",                    FALSE },
   { "key-0xd2",                    FALSE },
   { "key-0xd3",                    FALSE },
   { "key-0xd4",                    FALSE },
   { "key-0xd5",                    FALSE },
   { "key-0xd6",                    FALSE },
   { "key-0xd7",                    FALSE },
   { "key-0xd8",                    FALSE },
   { "key-0xd9",                    FALSE },
   { "key-0xda",                    FALSE },
   { "key-oem-4",                   TRUE  },
   { "key-oem-5",                   TRUE  },
   { "key-oem-6",                   TRUE  },
   { "key-oem-7",                   TRUE  },
   { "key-oem-8",                   TRUE  },
   { "key-0xe0",                    FALSE },
   { "key-oem-ax",                  FALSE },
   { "key-oem-102",                 FALSE },
   { "key-ico-help",                FALSE },
   { "key-ico-00",                  FALSE },
   { "key-processkey",              FALSE },
   { "key-ico-clear",               FALSE },
   { "key-packet",                  FALSE },
   { "key-0xe8",                    FALSE },
   { "key-oem-reset",               FALSE },
   { "key-oem-jump",                FALSE },
   { "key-oem-pa1",                 FALSE },
   { "key-oem-pa2",                 FALSE },
   { "key-oem-pa3",                 FALSE },
   { "key-oem-wsctrl",              FALSE },
   { "key-oem-cusel",               FALSE },
   { "key-oem-attn",                FALSE },
   { "key-oem-finish",              FALSE },
   { "key-oem-copy",                FALSE },
   { "key-oem-auto",                FALSE },
   { "key-oem-enlw",                FALSE },
   { "key-oem-backtab",             FALSE },
   { "key-attn",                    FALSE },
   { "key-crsel",                   FALSE },
   { "key-exsel",                   FALSE },
   { "key-ereof",                   FALSE },
   { "key-play",                    FALSE },
   { "key-zoom",                    FALSE },
   { "key-noname",                  FALSE },
   { "key-pa1",                     FALSE },
   { "key-oem-clear",               FALSE },
   { "key-0xff",                    FALSE },
};
