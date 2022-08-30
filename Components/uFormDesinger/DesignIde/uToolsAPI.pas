{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 1995-2010 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit uToolsAPI;

interface

{ !!! Please keep this unit's uses clause clean of *any* private IDE       !!! }
{ !!! units.  Before making modifications to this unit, please see Allen.  !!! }

{$IFDEF MSWINDOWS}
uses Windows, SysUtils, Classes, ActiveX, TypInfo, uDockForm, uDesignIntf, Menus,
  ActnListXE, Graphics, ImgList, Forms, Controls, ComCtrls, XMLIntf, IniFiles;
{$ENDIF}
{$IFDEF LINUX}
uses Types, SysUtils, Classes, TypInfo, uDockForm, uDesignIntf, Menus, ActnListXE,
  Graphics, ImgList, Forms, Controls, ComCtrls;
{$NOINCLUDE Menus}  
{$NOINCLUDE ActnList}
{$NOINCLUDE Graphics}
{$NOINCLUDE ImgList}
{$NOINCLUDE Forms}
{$NOINCLUDE ComCtrls}
{$ENDIF}

(*$HPPEMIT 'DEFINE_GUID(IID_IBorlandIDEServices70,0x7FD1CE92,0xE053,0x11D1,0xAB,0x0B,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IBorlandIDEServices,0xC9E8E577,0xB5D8,0x43F3,0xBC,0x84,0x6A,0x73,0x4A,0x01,0x57,0x32);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAComponent,0x34B2E2D1,0xE36F,0x11D1,0xAB,0x0E,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTACustomDrawMessage,0x589BBDA2,0xF995,0x11D1,0xAB,0x27,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTACustomMessage,0x589BBDA1,0xF995,0x11D1,0xAB,0x27,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTACustomMessage50,0xB7523AB7,0xEB81,0x11D2,0xAC,0x7B,0x00,0xC0,0x4F,0xB1,0x73,0xDC);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTACustomMessage100,0x824153E3,0x6336,0x48BA,0x80,0x5E,0x1A,0x35,0xE4,0x29,0x78,0x7E);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAFormEditor,0x56931EB9,0x329A,0xD411,0x87,0xC6,0x9B,0x27,0x30,0x41,0x22,0x00);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAServices,0x8209041F,0xF37F,0x4570,0x88,0xB8,0x6C,0x31,0x0F,0xFF,0xF8,0x1A);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAServices120,0x89160C3A,0x8EF4,0x4D2E,0x8F,0xD5,0xD8,0x49,0x2F,0x61,0xDB,0x3E);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAServices90,0x89160C3A,0x8EF4,0x4D2E,0x8F,0xD5,0xD8,0x49,0x2F,0x61,0xDB,0x3E);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAServices70,0xC17B3DF1,0xDFE5,0x11D2,0xA8,0xC7,0x00,0xC0,0x4F,0xA3,0x2F,0x53);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAServices40,0x3C7F3267,0xF0BF,0x11D1,0xAB,0x1E,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTACustomDockableForm,0xF8EF756F,0x4E95,0x4F1F,0xB2,0x29,0xA3,0xDE,0xF7,0xBC,0xC3,0x50);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAToDoItem,0x094003D8,0xE7AA,0x11D2,0xAA,0x99,0x00,0xC0,0x4F,0xA3,0x5C,0xE8);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAUnknown,0x8CC6430B,0xE721,0x11D2,0xA8,0xCC,0x00,0xC0,0x4F,0xA3,0x2F,0x53);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAActionServices,0xF17A7BC9,0xE07D,0x11D1,0xAB,0x0B,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAAddressBreakpoint,0x09063878,0xE43A,0x11D1,0xAB,0x0F,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTABreakpoint,0x8950E1C9,0xC32F,0x4132,0x87,0xDD,0x62,0x78,0x6A,0x3F,0x99,0x04);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTABreakpoint120,0x614D8D87,0xF3E2,0x46B7,0x80,0x33,0x4E,0x8B,0x37,0xE6,0x97,0xBB);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTABreakpoint80,0x446F637B,0x3EBD,0x4E33,0xB0,0x11,0x71,0x4C,0xE9,0x64,0x7B,0xB9);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTABreakpoint50,0x569EFCFB,0xC69B,0x11D2,0xAC,0x67,0x00,0xC0,0x4F,0xB1,0x73,0xDC);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTABreakpoint40,0x34B2E2D4,0xE36F,0x11D1,0xAB,0x0E,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTABreakpointNotifier,0x34B2E2D5,0xE36F,0x11D1,0xAB,0x0E,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTABufferOptions,0x8C748540,0xC6C1,0x11D2,0x81,0x39,0x00,0x60,0x97,0x92,0xF1,0x34);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTACodeInsightServices60,0x476904F8,0x89A9,0x4CD8,0xA7,0x1E,0x16,0x46,0x60,0x65,0x97,0x63);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTACodeInsightServices,0xEE5C42A9,0xDBC8,0x4C5D,0xB2,0x8E,0x52,0x80,0x24,0xCB,0xC9,0x7C);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAComponent,0xAC139ADF,0x329A,0xD411,0x87,0xC6,0x9B,0x27,0x30,0x41,0x22,0x00);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTACreator,0x6EDB9B9E,0xF57A,0x11D1,0xAB,0x23,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTACustomDrawCodeInsightViewer,0x32CA7B43,0x9AFC,0x49CF,0xAB,0xC9,0x7E,0xCD,0x77,0x24,0x88,0xD9);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTADebuggerNotifier,0x34B2E2D8,0xE36F,0x11D1,0xAB,0x0E,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTADebuggerNotifier90,0x68558E84,0xA7EC,0x499F,0xAD,0x08,0xCB,0x00,0x87,0x6A,0xC5,0xBE);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTADebuggerNotifier100,0xFE684C77,0x220C,0x4999,0xAC,0xFA,0xC4,0xC0,0xC7,0xFB,0x6A,0x42);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTADebuggerNotifier110,0x20E36B81,0xE987,0x4947,0xAA,0xFA,0xAC,0x0E,0x2F,0x0E,0x72,0xEF);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTADebuggerServices60,0x0E3B9D7A,0xE119,0x11D1,0xAB,0x0C,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTADebuggerServices90,0xA797823A,0x6BD7,0x41A4,0xB3,0x6B,0x3A,0x83,0x1A,0x73,0x7B,0x2D);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTADebuggerServices120,0x29AE42CE,0x006A,0x4A96,0xA0,0xBB,0x0D,0x63,0xD9,0xE8,0x3A,0x5C);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTADebuggerServices,0xDC682429,0xBB92,0x4AF7,0x9E,0x62,0x26,0x55,0x7D,0x68,0xDE,0x75);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTADebuggerVisualizer,0x744B7632,0x9F86,0x49B6,0xAE,0xD5,0x7A,0x48,0xDA,0x25,0xE3,0x76);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTADebuggerVisualizerValueReplacer,0x6BBFB765,0xE76F,0x449D,0xB0,0x59,0xA7,0x94,0xFA,0x06,0xF9,0x17);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTADebuggerVisualizerExternalViewerUpdater,0x4FA77EAB,0x4BA4,0x4203,0xB4,0x51,0x3B,0x3C,0x5B,0x42,0x8D,0x39);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTADebuggerVisualizerExternalViewer,0xA0D5CAF5,0x83A3,0x446E,0xB0,0x40,0xB3,0xE4,0x0A,0x92,0x6A,0x72);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAEditActions60,0xECB23623,0xE2D8,0x11D2,0xAB,0xE5,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAEditActions,0x9A073F11,0x2732,0xD611,0x95,0x8B,0x00,0xC0,0x4F,0xA0,0x6A,0xFC);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAElideActions120,0x3AB41d14,0xD290, 0x4674,0x89,0x78,0xD0,0xAB,0x4E,0x70,0xC1,0x4B);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAElideActions,0x8080361F,0x888F,0x4D0E,0x9E,0xCF,0x3F,0xE2,0x1F,0xA7,0x68,0xB5);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAEditBlock,0x9C510463,0xC7BC,0x11D2,0x9A,0xEB,0x00,0xA0,0x24,0x57,0x62,0x1F);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAEditWindow,0x8CC6430B,0xE721,0x11D2,0xA8,0xCC,0x00,0xC0,0x4F,0xA3,0x2F,0x53);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAEditBuffer60,0x9C510460,0xC7BC,0x11D2,0x9A,0xEB,0x00,0xA0,0x24,0x57,0x62,0x1F);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAEditBuffer,0xEB6465CE,0xD901,0x43C4,0xAB,0x69,0x24,0x0A,0x74,0x00,0xB9,0xAA);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAEditBufferIterator,0x8ECB33AA,0xD0BD,0x11D2,0xAB,0xD6,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAEditLineNotifier,0xD9D48F50,0xE6CC,0x11D2,0xAB,0xE8,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAEditLineTracker,0xD9D48F4F,0xE6CC,0x11D2,0xAB,0xE8,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAEditOptions60,0x487BEA91,0xDBC0,0x11D2,0xAB,0xDE,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAEditOptions,0x02999EF7,0x669C,0x406B,0x8E,0x14,0x4F,0xE8,0xB2,0x75,0x42,0xB8);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAEditor,0xF17A7BD0,0xE07D,0x11D1,0xAB,0x0B,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAEditorNotifier,0x0E3B9D7B,0xE119,0x11D1,0xAB,0x0C,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAEditorServices60,0xC2812BA7,0xC48D,0x11D2,0x9A,0xE8,0x00,0xA0,0x24,0x57,0x62,0x1F);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAEditorServices70,0x2596F557,0x44A3,0x49A6,0x86,0x7E,0x91,0xE2,0x1E,0x00,0xF5,0x3E);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAEditorServices80,0xF37E8C46,0x0A02,0x4FD4,0x8D,0x57,0xE5,0x5F,0x9A,0x57,0x83,0xEC);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAEditorServices,0xBE733055,0x5ED8,0x45B4,0xBA,0xB1,0x19,0xC4,0x6C,0x23,0x74,0x08);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAEditorServices,0x3CC6849A,0x6C72,0x49F8,0xBF,0x63,0xE9,0x50,0x83,0x78,0x91,0x41);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTACustomEditorView,0xE9465FAF,0xB671,0x4098,0x9E,0xD9,0xAE,0x4C,0x05,0xC5,0x45,0x4A);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTACustomEditorViewState,0xAA6AA3D4,0x1A63,0x4A93,0x89,0x64,0x45,0x04,0x84,0xF4,0xB4,0xD8);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTACustomEditorViewStatusPanel,0x39B8ECEB,0x3048,0x4733,0xBF,0x3D,0xC7,0x65,0x40,0xD8,0x7A,0xE2);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTACustomEditorSubView,0x655AA26C,0x5898,0x4DB5,0xB2,0x1F,0x4F,0x55,0xE9,0xB2,0xB4,0x07);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAEditorViewServices,0xCCE8FBE0,0xE121,0x450C,0x93,0x66,0x1A,0xC6,0xBF,0xD6,0xCF,0x81);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAEditPosition,0x9C510464,0xC7BC,0x11D2,0x9A,0xEB,0x00,0xA0,0x24,0x57,0x62,0x1F);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAEditReader,0x26EB0E4F,0xF97B,0x11D1,0xAB,0x27,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAEditView40,0x0E3B9D78,0xE119,0x11D1,0xAB,0x0C,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAEditView140,0x9C510462,0xC7BC,0x11D2,0x9A,0xEB,0x00,0xA0,0x24,0x57,0x62,0x1F);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAEditView,0x5EBBA00C,0x059D,0x4571,0xB3,0x0A,0x32,0x8D,0x12,0x64,0xBB,0xB8);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAEditWriter,0x26EB0E50,0xF97B,0x11D1,0xAB,0x27,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAEnvironmentOptions,0x9C0E91FB,0xFA5A,0x11D1,0xAB,0x28,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAFile,0x6E2AD9B0,0xF7F0,0x11D1,0xAB,0x26,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAFileSystem,0xA9D1389D,0xF4B0,0x11D1,0xAB,0x22,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAStreamModifyTime,0x49F2F63F,0x60CB,0x4FD4,0xB1,0x2F,0x81,0x67,0xFC,0x79,0xB2,0x93);*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAFormEditor,0xF17A7BD2,0xE07D,0x11D1,0xAB,0x0B,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAFormNotifier,0x0E3B9D7C,0xE119,0x11D1,0xAB,0x0C,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAFormWizard,0x36C8BF35,0xEFFE,0x11D1,0xAB,0x1D,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAFormWizard100,0x56D61347,0xC74D,0x4BF7,0x89,0xA9,0xF4,0x22,0xF3,0x1C,0xF2,0x59);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAIDENotifier,0xE052204F,0xECE9,0x11D1,0xAB,0x19,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAIDENotifier50,0xAC7D29F1,0xD9A9,0x11D2,0xA8,0xC1,0x00,0xC0,0x4F,0xA3,0x2F,0x53);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAIDENotifier80,0x41679BBC,0x660E,0x4948,0xAD,0x80,0x63,0xC6,0x79,0xCB,0x97,0x3C);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAKeyBindingServices,0xF8CAF8D8,0xD263,0x11D2,0xAB,0xD8,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAKeyboardBinding,0xF8CAF8D7,0xD263,0x11D2,0xAB,0xD8,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAKeyboardServices,0xF8CAF8D5,0xD263,0x11D2,0xAB,0xD8,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAKeyboardDiagnostics,0xAEFC65F1,0x2504,0x11D3,0xAC,0x25,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAKeyContext,0x3E7790CB,0xD2BB,0x11D2,0xAB,0xD8,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTACodeInsightManager,0xBA5B444A,0x6E78,0x4A79,0xBF,0x05,0xE1,0x84,0xC1,0x13,0x2B,0x30);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTACodeInsightParameterList,0x99B6A644,0x3E97,0x48A1,0x97,0x58,0x0A,0x5F,0xE9,0x47,0x67,0xC7);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTACodeInsightParamQuery,0xB1842926,0xC7F7,0x4869,0xB5,0x5A,0xCF,0xDB,0x6B,0xF7,0x05,0xB5);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTACodeInsightSymbolList,0x4CA1CDFD,0xBD9A,0x4628,0x94,0xAE,0x9B,0xF3,0xEB,0x2D,0xA2,0x2E);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTACodeInsightViewer,0xAAA55FAC,0x350E,0x4F43,0x9C,0x42,0x4F,0xC2,0x8B,0x6B,0xFE,0x33);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTACodeInsightViewerCanvas,0x8C60AB99,0x5BDB,0x44EE,0xB5,0xC8,0x33,0xCB,0x43,0x69,0x0D,0xF2);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAMenuWizard,0xB75C0CE2,0xEEA6,0x11D1,0x95,0x04,0x00,0x60,0x8C,0xCB,0xF1,0x53);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAMessageServices40,0x26EB0E4E,0xF97B,0x11D1,0xAB,0x27,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAMessageServices50,0x3263774B,0xE959,0x11D2,0xAC,0x7B,0x00,0xC0,0x4F,0xB1,0x73,0xDC);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAMessageServices60,0x58A40C76,0x7EC6,0x41DA,0xA2,0xEF,0x4B,0x3A,0xF3,0x1D,0x39,0x77);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAMessageServices70,0xB3F7D3A6,0xD1F7,0x48A0,0x8B,0xB0,0xF4,0x9C,0xF6,0x0F,0xB8,0x15);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAMessageServices80,0x02DD618A,0x30A0,0x4DCE,0x9D,0x04,0x6B,0x73,0x66,0x46,0xFF,0xCB);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAMessageServices,0x29E893DB,0xDD9A,0x4CEA,0xB2,0xEE,0x57,0x53,0x2E,0x01,0xA9,0xB9);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAMessageGroup80,0x233F4508,0x6022,0x4DDF,0xB6,0xD3,0xD2,0x10,0x8B,0xAf,0x80,0xDB);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAMessageGroup90,0xCF2B68C9,0x9ED0,0x461E,0xA5,0xF4,0xDF,0xC3,0xB0,0x26,0x8A,0x85);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAMessageGroup,0x52A6CDC6,0x2225,0x4D3C,0xAC,0x02,0xA6,0x8C,0x9B,0x19,0xA9,0x67);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAMessageNotifier,0xFDCB2ED4,0xB89C,0x4D00,0xB0,0xDB,0x19,0x56,0x29,0x51,0xCD,0xBB);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAMessageNotifier,0x0AE796BE,0xB5D7,0x4830,0x91,0x59,0xDE,0xFF,0x82,0xAC,0x07,0x6B);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAModule40,0xF17A7BCC,0xE07D,0x11D1,0xAB,0x0B,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAModule50,0x15D3FB81,0xEF27,0x488E,0xB2,0xB4,0x26,0xB5,0x9C,0xA8,0x9D,0x9D);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAModule70,0x2438BFB8,0xC742,0x48CD,0x8F,0x50,0xDE,0x6C,0x7F,0x76,0x4A,0x55);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAModule,0x7FF96161,0xE610,0x4414,0xB8,0xB1,0xD1,0xEC,0xA7,0x6F,0xEA,0xFB);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAModuleCreator,0x6EDB9B9A,0xF57A,0x11D1,0xAB,0x23,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAModuleInfo50,0xF17A7BD6,0xE07D,0x11D1,0xAB,0x0B,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAModuleInfo,0xB3EEB4D2,0xECDD,0x4CDC,0xB9,0x6E,0xB5,0xC8,0xF6,0xD0,0x50,0xA8);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAModuleNotifier,0xF17A7BCE,0xE07D,0x11D1,0xAB,0x0B,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAModuleNotifier80,0x6C4714BB,0x223A,0x4CDF,0xA7,0x10,0x42,0x9F,0xE8,0xFA,0x0B,0x91);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAModuleServices70,0xF17A7BCD,0xE07D,0x11D1,0xAB,0x0B,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAModuleServices,0x55A5E848,0x27FB,0x4880,0x8E,0x7C,0x7F,0x05,0xA9,0x80,0x24,0x82);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTANotifier,0xF17A7BCF,0xE07D,0x11D1,0xAB,0x0B,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAOptions,0x9C0E91FC,0xFA5A,0x11D1,0xAB,0x28,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAPackageServices,0x26EB0E4D,0xF97B,0x11D1,0xAB,0x27,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAProcess60,0x34B2E2D2,0xE36F,0x11D1,0xAB,0x0E,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAProcess70,0x64FC3321,0xBEC8,0x4E88,0xB1,0x7A,0x3E,0x78,0xEA,0x15,0xF1,0x0E);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAProcess90,0xBEBD67CA,0xF6FC,0x44A7,0xAC,0xBF,0xE3,0x14,0xDB,0x08,0x58,0x27);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAProcess,0x1C540740,0xE350,0x4DD1,0xB0,0x26,0xC2,0x33,0xD1,0xC3,0x80,0xD4);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAProcess,0xD65533D2,0x52B1,0x460F,0xAB,0xF1,0x48,0xCA,0x41,0x55,0x24,0xF5);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAProcessModNotifier,0x0906387A,0xE43A,0x11D1,0xAB,0x0F,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAProcessModule80,0x09063879,0xE43A,0x11D1,0xAB,0x0F,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAProcessModule90,0x9B4A6BC8,0xCC15,0x42A9,0xA4,0x1D,0x81,0x6A,0x72,0xCA,0x0A,0xF1);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAProcessModule110,0xEA1D9277,0xC318,0x4E5C,0x8B,0xDC,0x03,0x52,0x9E,0x81,0xDF,0x8E);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAProcessModule,0x41171E69,0xE830,0x40B4,0xA8,0xC6,0x1A,0x11,0x53,0x07,0xC8,0xA1);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAProcessNotifier,0x34B2E2D6,0xE36F,0x11D1,0xAB,0x0E,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAProcessNotifier90,0xE2725B23,0xE67C,0x4CF1,0xB9,0x28,0xFA,0x0F,0x5B,0x9C,0x2C,0x29);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAProject40,0xF17A7BCA,0xE07D,0x11D1,0xAB,0x0B,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAProject70,0x06C88136,0xF367,0x4D47,0xB8,0xB4,0xCC,0xAC,0xB3,0xD7,0x43,0x9A);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAProject90,0xBBBE4CC6,0x36DE,0x4986,0xBD,0x9E,0x9D,0xF0,0xF0,0x6F,0xC8,0xF1);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAProject,0xD0090018,0xD879,0x41FC,0x8F,0x83,0xAA,0x4F,0x40,0x09,0x8A,0xCF);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAProjectBuilder,0x08A5B1F5,0xFCDA,0x11D2,0xAC,0x82,0x00,0xC0,0x4F,0xB1,0x73,0xDC);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAProjectBuilder40,0xF17A7BD5,0xE07D,0x11D1,0xAB,0x0B,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAProjectCreator,0x6EDB9B9D,0xF57A,0x11D1,0xAB,0x23,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAProjectCreator50,0x64312F82,0x62F3,0x48E9,0xBA,0xF6,0xB0,0x3D,0xF4,0x50,0x31,0x2A);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAProjectCreator80,0x9A1D6AF5,0x84FA,0x481C,0xA4,0x46,0x74,0x6D,0x9A,0x50,0x53,0x3E);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAProjectGroup,0xF17A7BCB,0xE07D,0x11D1,0xAB,0x0B,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAProjectGroupCreator,0x6EDB9B9F,0xF57A,0x11D1,0xAB,0x23,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAProjectOptions,0xF899EBC6,0xE6E2,0x11D2,0xAA,0x90,0x00,0xC0,0x4F,0xA3,0x70,0xE9);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAProjectOptions40,0xF17A7BD4,0xE07D,0x11D1,0xAB,0x0B,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTABuildConfiguration,0x92A52A72,0xB0D2,0x4898,0x8A,0x20,0x29,0x81,0x32,0xF7,0x4C,0x16);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAProjectOptionsConfigurations,0xE158B38A,0x90BF,0x425E,0xA6,0x34,0x03,0x58,0xB7,0x94,0x87,0x0E);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAProjectResource,0x26EB0E52,0xF97B,0x11D1,0xAB,0x27,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAProjectWizard,0x36C8BF36,0xEFFE,0x11D1,0xAB,0x1D,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAProjectWizard100,0x809D578B,0xAE79,0x4CC2,0xA6,0xED,0xD7,0xA8,0xCD,0x24,0xC7,0x4D);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTARecord,0xF8CAF8D6,0xD263,0x11D2,0xAB,0xD8,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAReplaceOptions,0xD1766F8C,0xD915,0x11D2,0xA8,0xC1,0x00,0xC0,0x4F,0xA3,0x2F,0x53);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTARepositoryWizard,0xB75C0CE1,0xEEA6,0x11D1,0x95,0x04,0x00,0x60,0x8C,0xCB,0xF1,0x53);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTARepositoryWizard60,0x08FCCD88,0x3A21,0x4281,0xAD,0xC9,0x62,0xFC,0x03,0x4C,0xDD,0x12);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAResourceEntry,0x26EB0E51,0xF97B,0x11D1,0xAB,0x27,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTASearchOptions,0xD1766F8B,0xD915,0x11D2,0xA8,0xC1,0x00,0xC0,0x4F,0xA3,0x2F,0x53);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAServices50,0x7FD1CE91,0xE053,0x11D1,0xAB,0x0B,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAServices60,0x577ECE00,0x59EE,0x4F21,0x81,0x90,0x9F,0xD8,0xA4,0x5F,0xE5,0x50);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAServices70,0x0044BB24,0x425D,0xD611,0x9C,0xF1,0x00,0xC0,0x4F,0xA0,0x6A,0xFC);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAServices100,0x33B33186,0x3CEC,0x4624,0x97,0x0E,0x41,0x7A,0x8F,0xE1,0x40,0x89);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAServices,0x17A48937,0x2C9C,0x4543,0xAB,0x6D,0x2C,0xF1,0x3B,0xAE,0x54,0x4B);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTASourceBreakpoint,0x09063877,0xE43A,0x11D1,0xAB,0x0F,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTASourceEditor,0xF17A7BD1,0xE07D,0x11D1,0xAB,0x0B,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTASpeedSetting,0xB5CDCE07,0xE093,0x11D2,0xAB,0xE2,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAThread50,0x34B2E2D3,0xE36F,0x11D1,0xAB,0x0E,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAThread60,0x2646D502,0x95F8,0x4E6F,0xA1,0xEC,0x97,0x6E,0x96,0x63,0xC9,0xB6);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAThread70,0x24064FD3,0x5D3C,0xD611,0x88,0xBC,0x00,0xC0,0x4F,0xA0,0x6A,0xFC);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAThread90,0x175F985B,0x4F54,0x41B2,0xA0,0xA1,0x54,0xF3,0xB6,0x6E,0xCD,0x07);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAThread110,0x3A96CD8F,0xA5CD,0x4AFE,0x8A,0x73,0xDA,0xE1,0x26,0x50,0x95,0xD9);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAThread120,0xDF4C57A6,0x8674,0x4D7E,0xB7,0x5E,0x67,0x04,0xBB,0x5F,0x1A,0x54);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAThread,0xBC146984,0x1E20,0x4695,0x87,0x91,0x25,0xE6,0xA8,0x2F,0x52,0xF7);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAThread,0x381708B8,0xA0FA,0x44DC,0xB1,0x73,0x33,0x28,0xAE,0xFA,0x04,0x32);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAThreadNotifier,0x34B2E2D7,0xE36F,0x11D1,0xAB,0x0E,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAToDoManager,0x3D4A0565,0xEB77,0x11D2,0xAA,0x9A,0x00,0xC0,0x4F,0xA3,0x5C,0xE8);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAToDoServices,0xF8FC00EF,0xE61A,0x11D2,0xAA,0x99,0x00,0xC0,0x4F,0xA3,0x5C,0xE8);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAToolsFilterNotifier,0xCEF1F13A,0xE877,0x4F20,0x88,0xF2,0xF7,0xE2,0xBA,0x61,0xAA,0xF4); *)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAToolsFilter,0x8864B891,0x9B6D,0x4002,0xBB,0x2E,0x1D,0x6E,0x59,0xBF,0xA4,0x9A); *)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTATypeLibrary, 0x7A2F5910,0x58D2,0x448E,0xB4,0x57,0x2D,0xC0,0x1E,0x85,0x3D,0x46);*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTATypeLibEditor,0xF17A7BD3,0xE07D,0x11D1,0xAB,0x0B,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTATypeLibModule,0x0BBAEEA0,0xEF74,0x11D1,0xAB,0x1C,0x00,0xC0,0x4F,0xB1,0x6F,0xB3);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAWizard,0xB75C0CE0,0xEEA6,0x11D1,0x95,0x04,0x00,0x60,0x8C,0xCB,0xF1,0x53);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAWizardServices,0xB75C0CE3,0xEEA6,0x11D1,0x95,0x04,0x00,0x60,0x8C,0xCB,0xF1,0x53);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAHighlighter, 0x79D28DA1,0x42F6,0x44B9,0xAE,0x33,0xD0,0x01,0xFD,0x75,0xDC,0x40);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAHighlightServices, 0x78C26089,0x6CAD,0x40D1,0xBA,0xC2,0x37,0xA8,0x4D,0xF8,0xF3,0xE6);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAProjectFileStorageNotifier, 0xD6B7B13F,0xF5EA,0x4320,0xBD,0xCE,0x55,0x23,0x66,0x38,0xBD,0xE2);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAProjectFileStorage, 0x81515027,0xEEED,0x442F,0x97,0x7C,0x8F,0x39,0xF5,0x3D,0x8D,0x0A);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTASyncEditPoint100, 0x59492E58,0x8CE3,0x45D8,0x90,0x2F,0xD8,0x04,0x75,0xDE,0xFE,0x9D);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTASyncEditPoint, 0xB724144B,0xD292,0x4BAB,0x92,0x5F,0xBE,0x84,0xD7,0xE7,0x50,0x7C);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTASyncEditPoints, 0x5E7AD63A,0xCB78,0x4BF4,0x9F,0x1D,0xB2,0x57,0x68,0x98,0xDC,0xB4);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAPersonalityServices100, 0xF66FB6B3,0x24DC,0x4BC0,0x8A,0x6B,0x41,0x59,0xB5,0x27,0xA1,0xFC);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAPersonalityServices, 0x92D70AB5,0xF54F,0x4432,0x8E,0x0E,0x5B,0xEE,0xF4,0xB3,0xBE,0x77);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAPersonalityDevelopers, 0x765E768E,0xCF71,0x427C,0xAC,0x9C,0xCF,0x4B,0xFE,0xBC,0xFE,0xD5);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAProject120, 0x3D7E07CB,0x392D,0x4EFB,0x84,0x1D,0xA6,0xC6,0xE3,0x38,0xCF,0x13);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTACompileServices, 0x68C486EF,0xC079,0x4D40,0xB4,0x62,0x2C,0x0D,0xD2,0x1F,0xE3,0x42);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAMenuContext, 0x378F0D38,0xED5F,0x4128,0xB7,0xD6,0x9D,0x42,0x3F,0xC1,0x50,0x2F);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTALocalMenu, 0x83ECCBDF,0x939D,0x4F8D,0xB9,0x6D,0xA0,0xC6,0x7A,0xCC,0x86,0xEA);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAProjectMenuContext, 0xECEC33FD,0x837A,0x46DC,0xA0,0xAD,0x1F,0xFE,0xBE,0xEA,0x23,0xAF);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAProjectManagerMenu, 0x5E3B2F18,0x306E,0x4922,0x90,0x67,0x3F,0x71,0x84,0x3C,0x51,0xFA);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAIDEInsightService, 0xD1258D2C,0xDE95,0x4A0A,0x9A,0x7E,0x8C,0x61,0x67,0xF4,0x8B,0x31);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAIDEInsightNotifier, 0xFDEC7D0D,0x9633,0x424A,0xA9,0x25,0x5D,0xC1,0xDE,0x13,0xFD,0x48);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAIDEInsightItem, 0x539EE5EB,0x7A17,0x4756,0x9E,0x98,0x09,0xEE,0x71,0x55,0x1A,0xE0);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_IOTAIDEInsightCategory, 0xD943D3FE,0xBC3F,0x4405,0xBF,0x2A,0x4F,0x9F,0x00,0x1A,0xD1,0x9E);'*)

(*$HPPEMIT '' *)
(*$HPPEMIT '#ifdef __linux__' *)
(*$HPPEMIT '  /* provide dummy typedefs for the NTA types which may be referenced by */' *)
(*$HPPEMIT '  /* other interfaces.  The NTA interfaces are not available on Linux.  */' *)
(*$HPPEMIT '  namespace Toolsapi {' *)
(*$HPPEMIT '    typedef void* _di_INTAEditWindow;' *)
(*$HPPEMIT '    typedef void* _di_INTAComponent;' *)
(*$HPPEMIT '    typedef void* _di_INTAFormEditor;' *)
(*$HPPEMIT '    typedef void* _di_INTACustomDrawMessage;' *)
(*$HPPEMIT '    typedef void* _di_INTAServices40;' *)
(*$HPPEMIT '    typedef void* _di_INTAServices70;' *)
(*$HPPEMIT '    typedef void* _di_INTAServices90;' *)
(*$HPPEMIT '    typedef void* _di_INTAServices120;' *)
(*$HPPEMIT '    typedef void* _di_INTAServices;' *)
(*$HPPEMIT '    typedef void* _di_INTACustomDockableForm;' *)
(*$HPPEMIT '    typedef void* _di_INTAToDoItem;' *)
(*$HPPEMIT '    typedef void* _di_INTACustomDrawCodeInsightViewer;' *)
(*$HPPEMIT '    typedef void* _di_INTACustomEditorView;' *)
(*$HPPEMIT '    typedef void* _di_INTACustomEditorViewState;' *)
(*$HPPEMIT '    typedef void* _di_INTACustomEditorViewStatusPanel;' *)
(*$HPPEMIT '    typedef void* _di_INTACustomEditorSubView;' *)
(*$HPPEMIT '  } /* namespace Toolsapi */ ' *)
(*$HPPEMIT '#endif /* __linux__ */' *)
(*$HPPEMIT '' *)

{ OTA Prefix - Open Tools API
  Accessing these interfaces only requires that the user use the Borlndmm.dll via
  sharemem. }
{ NTA Prefix - Native Tools API
  Accessing these interfaces requires that the user compile requiring vclxx.bpl
  since actual objects are passed among the interfaces. }

const
  utForm          = 0;
  utDataModule    = 1;
  utProjUnit      = 2;
  utUnit          = 3;
  utRc            = 4;
  utAsm           = 5;
  utDef           = 6;
  utObj           = 7;
  utRes           = 8;
  utLib           = 9;
  utTypeLib       = 10;
  utPackageImport = 11;
  utFormResource  = 12;
  utNoMake        = 13;

  atWhiteSpace     = 0;
  atComment        = 1;
  atReservedWord   = 2;
  atIdentifier     = 3;
  atSymbol         = 4;
  atString         = 5;
  atNumber         = 6;
  atFloat          = 7;
  atOctal          = 8; // not used in Pascal tokenizer
  atHex            = 9;
  atCharacter      = 10; // not used in Pascal tokenizer
  atPreproc        = 11;
  atIllegal        = 12; // not used in Pascal tokenizer
  atAssembler      = 13;
  SyntaxOff        = 14;

  MarkedBlock      = 15;
  SearchMatch      = 16;

  atHotLink        = 17;

  atTags           = 20;
  atAttrNames      = 21;
  atAttrValues     = 22;
  atScripts        = 23;

  RightMargin      = 37;

  lfCurrentEIP          = $0001;
  lfBreakpointEnabled   = $0002;
  lfBreakpointDisabled  = $0004;
  lfBreakpointInvalid   = $0008;
  lfErrorLine           = $0010;
  lfBreakpointVerified  = $0020;
  lfBackgroundBkpt      = $0040; // Breakpoint only valid in a non-active process
  lfBackgroupEIP        = $0080; // Execution point only valid in a non-active process

  { RegionKind constants }

  rkRegion = 0;
  rkIf = 1;
  rkNameSpace = 2;
  rkType = 3;
  rkMethod = 4;
  rkNestedMethod = 5;
  rkGlobal = 6;

  // The following string constants are the internal names of editor macro
  // "scripts" that get executed in response to certain menu commands.  This
  // allows a menu command to execute the exact same internal "macro script"
  // as the direct key binding.

  mcGetFindString = 'GetFindString';
  mcReplace = 'Replace';
  mcRepeatSearch = 'RepeatSearch';
  mcIncrementalSearch = 'IncrementalSearch';
  mcGotoLine = 'GotoLine';
  mcClipCut = 'ClipCut';
  mcClipCopy = 'ClipCopy';
  mcClipPaste = 'ClipPaste';
  mcClipClear = 'ClipClear';
  mcHelpKeywordSearch = 'HelpKeywordSearch';
  mcOpenFileAtCursor = 'OpenFileAtCursor';
  mcToggleBreakpoint = 'ToggleBreakpoint';
  mcRunToHere = 'RunToHere';
  mcUndo = 'Undo';
  mcRedo = 'Redo';
  mcModify = 'Modify';
  mcAddWatchAtCursor = 'AddWatchAtCursor';
  mcInspectAtCursor = 'InspectAtCursor';
  mcSetMark0 = 'setMark0';
  mcSetMark1 = 'setMark1';
  mcSetMark2 = 'setMark2';
  mcSetMark3 = 'setMark3';
  mcSetMark4 = 'setMark4';
  mcSetMark5 = 'setMark5';
  mcSetMark6 = 'setMark6';
  mcSetMark7 = 'setMark7';
  mcSetMark8 = 'setMark8';
  mcSetMark9 = 'setMark9';
  mcMoveToMark0 = 'moveToMark0';
  mcMoveToMark1 = 'moveToMark1';
  mcMoveToMark2 = 'moveToMark2';
  mcMoveToMark3 = 'moveToMark3';
  mcMoveToMark4 = 'moveToMark4';
  mcMoveToMark5 = 'moveToMark5';
  mcMoveToMark6 = 'moveToMark6';
  mcMoveToMark7 = 'moveToMark7';
  mcMoveToMark8 = 'moveToMark8';
  mcMoveToMark9 = 'moveToMark9';

  // the following constant defines the name of the IDE's editor keymapping
  // table.  This keyboard is *always* defined and should not be popped
  // from the stack.

  sEditor = 'editor';

  // The following constants define the currently available form designers.
  // Use dAny for a wizard that doesn't care under which designer
  // it is invoked.

  dVCL = 'dfm';
  dCLX = 'xfm';
  dVCLNet = 'nfm';
  dDotNet = '.NET';
  dHTML = 'HTML';
  dAny = 'Any';

  WizardEntryPoint = 'INITWIZARD0001';
  isWizards = 'Wizards';

  { IDE's Toolbar names }
  sCustomToolBar = 'CustomToolBar';
  sStandardToolBar = 'StandardToolBar';
  sDebugToolBar = 'DebugToolBar';
  sViewToolBar = 'ViewToolBar';
  sDesktopToolBar = 'DesktopToolBar';
  sInternetToolBar = 'InternetToolBar'; // deprecated
  sCORBAToolBar = 'CORBAToolBar'; // deprecated
  sAlignToolbar = 'AlignToolbar';
  sBrowserToolbar = 'BrowserToolbar';
  sHTMLDesignToolbar = 'HTMLDesignToolbar';
  sHTMLFormatToolbar = 'HTMLFormatToolbar';
  sHTMLTableToolbar = 'HTMLTableToolbar';
  sPersonalityToolBar = 'PersonalityToolBar';
  sPositionToolbar = 'PositionToolbar';
  sSpacingToolbar = 'SpacingToolbar';

  { Default IDE application/project types }
  sApplication = 'Application';
  sLibrary = 'Library';
  sConsole = 'Console';
  sPackage = 'Package';
  sOptionSet = 'OptionSet';

  { Default IDE Module/Unit types }
  sUnit = 'Unit'; // Raw no form designer unit/C++ module and skeleton code
  sForm = 'Form'; // Unit/C++ module with a form designer
  sText = 'Text'; // Raw single file module with no skeleton code

  { C# module/application types }
  sCSApplication = sApplication;
  sCSLibrary = sLibrary;
  sCSConsole = sConsole;
  sCSPackage = sPackage; // same as sLibrary
  sAssembly = 'Assembly'; // Equivalent to sLibrary
  sUserControl = 'UserControl'; // When used as an application type, a new user control assembly is created
                                // when used as a module type, a new user control module is created
  sClass = 'Class'; // Used to create a new module source as a generic class
  sWinForm = 'WinForm'; // Equivalent to sForm

  { C++ Application Types }
  sCppConsoleExe = 'CppConsoleApplication'; // unmanaged console application
  sCppGuiApplication = 'CppGuiApplication'; // unmanaged windows GUI application
  sCppVCLApplication = 'CppVCLApplication'; // VCL Forms Application
  sCppDynamicLibrary = 'CppDynamicLibrary'; // unmanaged dynamic-link library
  sCppPackage = 'CppPackage'; // package
  sCppStaticLibrary = 'CppStaticLibrary'; // Static library
  sCppManagedConsoleExe = 'CppManagedConsoleApp'; // managed console application
  sCppManagedDll = 'CppManagedDynamicLibrary'; // managed dynamic-link library

  { TMoveCursorMasks }
  { The move cursor mask can be built from one of the following
    mmSkipWord       - Skip to the next alphanumeric character
    mmSkipNonWord    - Skip to the next non-alphanumeric character
    mmSkipWhite      - Skip to the next whitespace character (space, tab, newline)
    mmSkipNonWhite   - Skip to the next non-whitespace character
    mmSkipSpecial    - Skip to the next non-alphanumeric and non-whitespace character
    mmSkipNonSpecial - Skip to the next alphanumeric or whitespace character

    The following can be combined with any of the above
    mmSkipLeft       - Move to the left or toward the beginning of the file
    mmSkipRight      - Move to the right or toward the end of the file

    The following can be combined with any of the above
    mmSkipStream     - Ignore line ends when skipping.  When used with
                       mmSkipWhite, and the cursor is at the beginning or end
                       of a line, the cursor will continue to move until it
                       reaches a non-white character, non-EOL character, BOF or
                       EOF }
  mmSkipWord       = $00;
  mmSkipNonWord    = $01;
  mmSkipWhite      = $02;
  mmSkipNonWhite   = $03;
  mmSkipSpecial    = $04;
  mmSkipNonSpecial = $05;
  mmSkipLeft       = $00;
  mmSkipRight      = $10;
  mmSkipStream     = $20;

  { TCodeCompleteStyle }
  { The code completion style can be built from one the following
    csCodeList     - Invoke the IDE's CodeInsight Code completion function
    csParamList    - Invoke the IDE's CodeInsight Code parameters function

    One of the above can be combined with the following
    csManual       - This will cause the operation to be invoked immediately,
                     otherwise the IDE will use the delay timer set through the
                     Code Insight option before invoking the operation.
  }

  csCodelist       = $01;
  csParamList      = $02;
  csManual         = $80;

  { TKeyBindingFlags }
  { When assign a keybinding you may pass a combination of the following flags.
      NOTE: This *only* affects the terminal key (the last in a multi-key
            sequence).
    kfImplicitShift    - if the keycode is alpha, assign both the upper and
                         lower case versions.
    kfImplicitModifier - <Ctrl+k><Ctrl+b> = <Ctrl+K><b>
    kfImplicitKeypad   - When an assignment is made to a sequence with a
                         numeric keypad (Keypad) equivalent, such as PageUp,
                         a second assignment is implicitly made for the
                         equivalent
  }
  kfImplicitShift = $01;
  kfImplicitModifier = $02;
  kfImplicitKeypad = $04;

  { TRipFlags }
  { When calling RipText use these flags to control the most common character
    sets to include is the text ripped from the editor.

    rfBackward               - Rip the text backward or toward the beginning of the file
    rfInvertLegalChars       - Include in ripped text characters that are *not* in the set
    rfIncludeUpperAlphaChars - Automatically include all the upper case alpha characters
    rfIncludeLowerAlphaChars - Automatically include all the lower case alpha characters
    rfIncludeAlphaChars      - Automatically include all the upper and lower case alpha characters
    rfIncludeNumericChars    - Automatically include all the numeric characters
    rfIncludeSpecialChars    - Automatically include all the special characters such as "()[]..."
  }

  rfBackward               = $0100;
  rfInvertLegalChars       = $1000;
  rfIncludeUpperAlphaChars = $0001;
  rfIncludeLowerAlphaChars = $0002;
  rfIncludeAlphaChars      = $0003;
  rfIncludeNumericChars    = $0004;
  rfIncludeSpecialChars    = $0008;

  { Possible values for TOTAModuleType }

  omtForm          = 0;
  omtDataModule    = 1;
  omtProjUnit      = 2;
  omtUnit          = 3;
  omtRc            = 4;
  omtAsm           = 5;
  omtDef           = 6;
  omtObj           = 7;
  omtRes           = 8;
  omtLib           = 9;
  omtTypeLib       = 10;
  omtPackageImport = 11;
  omtFormResource  = 12;
  omtCustom        = 13;
  omtIDL           = 14;

  { This is the default personality that is used to register default file
    personality traits. }
  sDefaultPersonality = 'Default.Personality';
  { The following are Borland created personalities }
  sDelphiPersonality = 'Delphi.Personality';
  sDelphiDotNetPersonality = 'DelphiDotNet.Personality';
  sCBuilderPersonality = 'CPlusPlusBuilder.Personality';
  sCSharpPersonality = 'CSharp.Personality';
  sVBPersonality = 'VB.Personality';
  sDesignPersonality = 'Design.Personality';
  sGenericPersonality = 'Generic.Personality';


  { Gallery Categories }
  { You can now add your wizards to specific categories in the Gallery.
    You must first register or find your category before using it.
    The following categories will (probably) exist. }
  sCategoryRoot = 'Borland.Root';
  sCategoryGalileoOther = 'Borland.Galileo.Other';
  sCategoryDelphiNew = 'Borland.Delphi.New';
  sCategoryDelphiNewFiles = 'Borland.Delphi.NewFiles';
  sCategoryDelphiDotNetNew = 'Borland.Delphi.NET.New';
  sCategoryDelphiDotNetNewFiles = 'Borland.Delphi.NET.NewFiles';
  sCategoryCBuilderNew = 'Borland.CBuilder.New';
  sCategoryCBuilderNewFiles = 'Borland.CBuilder.NewFiles';
  sCategoryCurrentProject = 'Borland.CurrentProject';
  sCategoryCSharpNew = 'Borland.CSharp.New';
  sCategoryCSharpNewFiles = 'Borland.CSharp.NewFiles';
  sCategoryMarkupNew = 'Borland.Markup.New';
  sCategoryMarkupNewFiles = 'Borland.Markup.NewFiles';
  sCategoryVBNew = 'Borland.VB.New';
  sCategoryVBNewFiles = 'Borland.VB.NewFiles';
  sCategoryNewUnitTest = 'UnitTest.Test';


  { IOTAEditOptions now are associated with file types. See
    IOTAEditorServices for more information }
  cDefEdOptions = 'Borland.EditOptions.';
  cDefEdDefault = cDefEdOptions + 'Default';
  cDefEdPascal = cDefEdOptions + 'Pascal';
  cDefEdC = cDefEdOptions + 'C';
  cDefEdCSharp = cDefEdOptions + 'C#';
  cDefEdHTML = cDefEdOptions + 'HTML';
  cDefEdXML = cDefEdOptions + 'XML';
  cDefEdSQL = cDefEdOptions + 'SQL';
  cDefEdIDL = cDefEdOptions + 'IDL';
  cDefEdVisualBasic = cDefEdOptions + 'VisualBasic';
  cDefEdJavaScript = cDefEdOptions + 'JavaScript';
  cDefEdStyleSheet = cDefEdOptions + 'StyleSheet';
  cDefEdINI = cDefEdOptions + 'INI';
  cDefEdPHP = cDefEdOptions + 'PHP';

  { Designer command string constants.  These are strings so that new commands
    can be added without affecting the interfaces.  This allows commands to be
    added without affecting the IDE core. }

  dcAlign = 'Align';

  dcSize = 'Size';
  dcScale = 'Scale';
  dcTabOrder = 'TabOrder';
  dcCreationOrder = 'CreationOrder';
  dcLockControls = 'LockControls';
  dcFlipChildrenAll = 'FlipChildrenAll';
  dcFlipChildrenSelected = 'FilpChildrenSelected';

  { Use these constants calling INTAEditWindow.CreateDockableForm in order to
    fulfill a loose contract with all personalities who wish to implement a
    specific type of functionality.  For instance, a personality (or group of
    personalities) may ask to create a Borland.CodeExplorer dockable window.
    Then all subsequent personalities that ask to the Borland.CodeExplorer will
    get this same window.  This allows a *type* of window to share the same
    space with all the other personalities. }

  sBorlandEditorCodeExplorer = 'BorlandEditorCodeExplorer';

 { Some of the preset identifers that could be passed to
  INTAProjectMenuCreatorNotifier.  Other values could be file names.
 }
  sBaseContainer = 'BaseContainer';
  sFileContainer = 'FileContainer';
  sProjectContainer = 'ProjectContainer';
  sProjectGroupContainer = 'ProjectGroupContainer';
  sCategoryContainer = 'CategoryContainer';
  sDirectoryContainer = 'DirectoryContainer';
  sReferencesContainer = 'References';
  sContainsContainer = 'Contains';
  sRequiresContainer = 'Requires';
  sVirtualFoldContainer = 'VirtualFold';
  sBuildConfigContainer = 'BuildConfig';
  sOptionSetContainer = 'OptionSet';

  vvfPrivate = $00;
  vvfProtected = $01;
  vvfPublic = $02;
  vvfPublished = $03;
  vvfVisMask = $04;
  vvfDeprecated = $08;

  sBaseConfigurationKey = 'Base';  

  { These constants can be used to specify the priority of an INTACustomEditorSubView }
  svpHighest = Low(Integer);
  svpHigh = -255;
  svpNormal = 0;
  svpLow = 255;
  svpLowest = High(Integer);

  { Project manager menu item position constants.  Menus are built in numeric order }
  pmmpBuild = 1000000;
  pmmpOpen = 2000000;
  pmmpVersionControl = 3000000;
  pmmpAdd = 4000000;
  pmmpRemove = 5000000;
  pmmpSave = 6000000;
  pmmpRename = 7000000;
  pmmpUtils = 8000000;
  pmmpReorder = 9000000;
  pmmpOptions = 10000000;
  pmmpBuildConfig = 11000000;

  { Base user offset }
  pmmpUserOffset = 500000;

  { These constants should be used by addins in order to avoid collisions with built-in menu items.
    If a collision occurs, the order of the menu items may not be predictable }
  pmmpUserBuild = pmmpBuild + pmmpUserOffset;
  pmmpUserOpen = pmmpOpen + pmmpUserOffset;
  pmmpUserVersionControl = pmmpVersionControl + pmmpUserOffset;
  pmmpUserAdd = pmmpAdd + pmmpUserOffset;
  pmmpUserRemove = pmmpRemove + pmmpUserOffset;
  pmmpUserSave = pmmpSave + pmmpUserOffset;
  pmmpUserRename = pmmpRename + pmmpUserOffset;
  pmmpUserUtils = pmmpUtils + pmmpUserOffset;
  pmmpUserReorder = pmmpReorder + pmmpUserOffset;
  pmmpUserOptions = pmmpOptions + pmmpUserOffset;
  pmmpUserBuildConfig = pmmpUserOffset + pmmpBuildConfig;

type
  { ENonAIRException exceptions, when unhandled, will not show the user an exception
    dialog with a stack trace and will not allow the user to submit an Automated
    Incident Report (AIR) to Quality Central.  Exceptions in IDE addins that are
    intentionally left unhandled should be of this type (or of a type that is
    derived from this type) }
  ENonAIRException = class(Exception);

  { Exception class raised for any Personality Manager related exceptions }
  EPersonalityException = class(ENonAIRException);

  {
    cmOTAMake - Normal make
    cmOTABuild - Builds all modules that have source.
    cmOTACheck - Normal make without final link
    cmOTAMakeUnit - Valid only on an IOTAModule and in C++Builder
  }
  TOTACompileMode = (cmOTAMake, cmOTABuild, cmOTACheck, cmOTAMakeUnit);

  {
    crOTAFailed - the compile/build failed
    crOTASucceeded - the compile/build was successful
    crOTABackground - the compile/build was started in a background thread.
      Register an IOTACompileNotifier to be informed when it is finished
  }
  TOTACompileResult = (crOTAFailed, crOTASucceeded, crOTABackground);

  TOTAModuleType = type Integer;
  TOTAHandle = Pointer;

  TOTAToDoPriority = 0..5;

  { Editor position expressed as column/line after tabs are expanded to spaces
    and include the "virtual" editor space (columns beyond the end of lines) }
  TOTAEditPos = packed record
    Col: SmallInt; { Col is one-based }
    Line: Longint; { Line is one-based }
  end;

  { Editor position expressed as character index/line before tabs are expanded
    and does not include the indices beyond the end of a line }
  TOTACharPos = packed record
    CharIndex: SmallInt; { CharIndex is zero-based }
    Line: Longint; { Line is one-based }
  end;

  { Available option name expressed as a name and a type }
  TOTAOptionName = record
    Name: string;
    Kind: TTypeKind;
  end;

  { Dynamic array of option names }
  TOTAOptionNameArray = array of TOTAOptionName;

{$IFDEF MSWINDOWS}
  TOTAThreadContext = Windows.TContext;
{$ENDIF}
{$IFDEF LINUX}
  TFloatingSaveArea = record
    ControlWord: DWORD;
    StatusWord: DWORD;
    TagWord: DWORD;
    ErrorOffset: DWORD;
    ErrorSelector: DWORD;
    DataOffset: DWORD;
    DataSelector: DWORD;
    RegisterArea: array[0..79] of Byte;
    Cr0NpxState: DWORD;
  end;

  TOTAThreadContext = record
    ContextFlags: DWORD;
    Dr0: DWORD;
    Dr1: DWORD;
    Dr2: DWORD;
    Dr3: DWORD;
    Dr6: DWORD;
    Dr7: DWORD;
    FloatSave: TFloatingSaveArea;
    SegGs: DWORD;
    SegFs: DWORD;
    SegEs: DWORD;
    SegDs: DWORD;
    Edi: DWORD;
    Esi: DWORD;
    Ebx: DWORD;
    Edx: DWORD;
    Ecx: DWORD;
    Eax: DWORD;
    Ebp: DWORD;
    Eip: DWORD;
    SegCs: DWORD;
    EFlags: DWORD;
    Esp: DWORD;
    SegSs: DWORD;
  end;
{$ENDIF}

  TOTAXMMReg = packed record
    case Integer of
      0: (ByteReg: packed array[0..15] of Byte);
      1: (WordReg: packed array[0..7] of Word);
      2: (LongReg: packed array[0..3] of LongWord);
      3: (Int64Reg: packed array[0..1] of Int64);
// not yet: 4: (UInt64Reg: packed array[0..1] of UInt64);
      5: (SingleReg: packed array[0..3] of Single);
      6: (DoubleReg: packed array[0..1] of Double);
  end;

  TOTAXMMRegs = packed record
    XMM0: TOTAXMMReg;
    XMM1: TOTAXMMReg;
    XMM2: TOTAXMMReg;
    XMM3: TOTAXMMReg;
    XMM4: TOTAXMMReg;
    XMM5: TOTAXMMReg;
    XMM6: TOTAXMMReg;
    XMM7: TOTAXMMReg;
    MXCSR: LongWord;
  end;

  IOTAProject = interface;
  IOTAModule = interface;
  IOTANotifier = interface;
  IOTAEditView = interface;
  IOTAEditBuffer = interface;
  IOTAFormEditor = interface;
  IOTAComponent = interface;
  IBorlandIDEServices = interface;
  IOTAEditOptions = interface;
  IOTAEditorServices = interface;
  IOTAKeyboardServices = interface;
  IOTAKeyContext = interface;
  IOTAEditBlock = interface;

  { TBindingType - Indicates to the IDE how to manage this keybinding interface
     btComplete  - This keybinding defines a complete keybinding for the editor.
                   It is mutually exclusive of *all* other btComplete bindings.
                   All pre-defined internal keymaps are btComplete.
     btPartial   - This binding only implements a partial binding.  Many of
                   these may be registered and enabled as the user selects.  The
                   order of registration is determined by the user through the
                   IDE in the Tools|Editor Options dialog, Key Mappings page.
  }
  TBindingType = (btComplete, btPartial);

  { TKeyBindingResult }
  { krUnhandled - Return this if the key is not to be handled by this proc.
                  Will execute the next handler assigned to this key.  This will
                  also allow any menu shortcuts to execute if no other handlers
                  handle the key.
    krHandled   - This proc handled the key and no further processing is
                  required.  No menu shortcuts will be processed
    krNextProc  - This proc handled the key but execute the next handler if one
                  exists. No menu shortcuts will be processed }
  TKeyBindingResult = (krUnhandled, krHandled, krNextProc);

  { Keybinding proc }
  TKeyBindingProc = procedure (const Context: IOTAKeyContext; KeyCode: TShortcut;
    var BindingResult: TKeyBindingResult) of object;

  TMoveCursorMasks = Byte;

  { TSearchDirection }
  TSearchDirection = (sdForward, sdBackward);

  { IOTAStrings }

  { Base interface for typical TStrings access. }
  IOTAStrings = interface(IInterface)
    ['{36A00C9A-58D9-42EE-8B10-478C1B6691E8}']
    procedure Assign(const Strings: IOTAStrings);
    function GetCount: Integer;
    function GetData(const Index: Integer): Integer;
    function GetItem(const Index: Integer): string;
    function GetName(const Index: Integer): string;
    function GetValue(const Name: string): string;
    function GetValueFromIndex(const Index: Integer): string;
    procedure SetData(const Index: Integer; Value: Integer);
    procedure SetItem(const Index: Integer; const Value: string);
    procedure SetValue(const Name, Value: string);
    procedure SetValueFromIndex(const Index: Integer; const Value: string);

    property Count: Integer read GetCount;
    property Data[const Index: Integer]: Integer read GetData write SetData;
    property Items[const Index: Integer]: string read GetItem write SetItem;
    property Names[const Index: Integer]: string read GetName;
    property Values[const Name: string]: string read GetValue write SetValue;
    property ValueFromIndex[const Index: Integer]: string read GetValueFromIndex write SetValueFromIndex;
  end;

  INTAStrings = interface(IOTAStrings)
    ['{FDAFB316-82E7-47B3-A282-D7605ADF2AAF}']
    function GetStrings: TStrings;

    property Strings: TStrings read GetStrings;
  end;

  IOTANotifier = interface(IUnknown)
    ['{F17A7BCF-E07D-11D1-AB0B-00C04FB16FB3}']
    { This procedure is called immediately after the item is successfully saved.
      This is not called for IOTAWizards }
    procedure AfterSave;
    { This function is called immediately before the item is saved. This is not
      called for IOTAWizard }
    procedure BeforeSave;
    { The associated item is being destroyed so all references should be dropped.
      Exceptions are ignored. }
    procedure Destroyed;
    { This associated item was modified in some way. This is not called for
      IOTAWizards }
    procedure Modified;
  end;

  IOTAEditorNotifier = interface(IOTANotifier)
    ['{0E3B9D7B-E119-11D1-AB0C-00C04FB16FB3}']
    { Called when a new edit view is created(opInsert) or destroyed(opRemove) }
    procedure ViewNotification(const View: IOTAEditView; Operation: TOperation);
    { Called when a view is activated }
    procedure ViewActivated(const View: IOTAEditView);
  end;

  IOTAFormNotifier = interface(IOTANotifier)
    ['{0E3B9D7C-E119-11D1-AB0C-00C04FB16FB3}']
    { Called when the given form is activated }
    procedure FormActivated;
    { This is called immediately prior to the form being streamed out.  This
      may be called without first getting a BeforeSave as in the case of
      the project being compiled. }
    procedure FormSaving;
    { Called when a component on this form was renamed }
    procedure ComponentRenamed(ComponentHandle: TOTAHandle;
      const OldName, NewName: string);
  end;

  IOTAEditor = interface(IUnknown)
    ['{F17A7BD0-E07D-11D1-AB0B-00C04FB16FB3}']
    { Call this to register an IOTANotifier. The result is the index to be
      used when calling RemoveNotifier. If <0 then an error occurred. }
    function AddNotifier(const ANotifier: IOTANotifier): Integer;
    { Returns the actual filename of this module editor. Rename through
      IOTAModule}
    function GetFileName: string;
    { Returns the editor specific modified status }
    function GetModified: Boolean;
    { Returns the associated IOTAModule }
    function GetModule: IOTAModule;
    { Mark this editor modified.  The associated module will also be modified }
    function MarkModified: Boolean;
    { Call with the index obtained from AddNotifier }
    procedure RemoveNotifier(Index: Integer);
    { Show this editor.  If no views are active, at least one will be created }
    procedure Show;

    property FileName: string read GetFileName;
    property Modified: Boolean read GetModified;
    property Module: IOTAModule read GetModule;
  end;

  { If an IOTAEditor implements this interface, then it supports direct access
    to the memory buffer used to store the editor content }
  IOTAEditorContent = interface(IInterface)
    ['{748FB436-59B9-495D-8AC8-807CF6622967}']
    { Returns the current buffer content for the corresponding editor }
    function GetContent: IStream;
    { Sets the current buffer content for the corresponding editor.  NOTE:
      this will do a wholesale replacement of the content and all previous
      contents *will* be lost.  Use this at your own risk. You have been warned }
    procedure SetContent(const AStream: IStream);
    { Returns the current age of the buffer.  This is the datetime of the last
      actual modification of the file.  If the file has not been modified or has
      already been saved, this will match the file time on disk. }
    function GetContentAge: TDateTime;
    { Call this function to reset the internal setting of the editor's content
      disk age value.  The disk age is the age of the time when it was initially
      loaded from disk or the last time the file was saved. }
    procedure ResetDiskAge;

    property Content: IStream read GetContent write SetContent;
  end;

  IOTAToolsFilterNotifier = interface(IOTANotifier)
    ['{CEF1F13A-E877-4F20-88F2-F7E2BA61AAF4}']
    { Used by tools filter to fill message view with build result information. }
    { FileName is the name of the file that was compiled. }
    { ErrorCode is the error code result from the compile. }
    { StdOut is a string list of the standard out while the tool was executed. }
    { StdError is a string list of the standard error while the tool was executed. }
    procedure Filter(FileName: string; ErrorCode: Integer;
      StdOut,  StdError: TStrings);
    { Should return an unique name for the filter. }
    function GetFilterName: string;
  end;

  IOTAToolsFilter60 = interface(IUnknown)
    ['{8864B891-9B6D-4002-BB2E-1D6E59BFA49A}']
    { Call this to register an IOTANotifier. The result is the index to be
      used when calling RemoveNotifier. If <0 then an error occurred. }
    function AddNotifier(const ANotifier: IOTANotifier): Integer;
    { Call with the index obtained from AddNotifier }
    procedure RemoveNotifier(Index: Integer);
  end;

  IOTAToolsFilter = interface(IOTAToolsFilter60)
    ['{891B2757-FC6F-4DE9-B653-F55A52733324}']
    function FindFilter(const Name: string): IUnknown;
  end;

  { Use the IOTAEditReader class to gain read access to an editor buffer:

    NOTES:
     o The buffer is accessed as a linear "file" with line breaks included.
     o This reader interface could be accessed through a custom read-only
       TStream descendant.
     o "Buffer" will contain UTF8-encoded data if the editor contains Unicode
       characters

    WARNING!!!
     o A IOTAEditReader should never be active at the same time as an IOTAEditWriter.
  }

  IOTAEditReader = interface(IUnknown)
    ['{26EB0E4F-F97B-11D1-AB27-00C04FB16FB3}']
    function GetText(Position: Longint; Buffer: PAnsiChar; Count: Longint): Longint;
  end;

  { Use the IOTAEditWriter class to gain write access to an editor buffer:

    NOTES:
     o As with the reader, the buffer is accessed as a linear "file" with
       line breaks included.  The writer uses a "copy in place" metaphor for
       modifying the editor buffer.  In other words, the writer can be thought
       of as simply copying from one buffer to another.  All positions (Pos)
       passed to the function are positions relative to the original file.  Due
       to the "copy" metaphor of the writer it does not support moving backward
       in the editor buffer. It is recommended that all modifications that must
       be performed should be done from the start to the finish.
     o After the IOTAEditWriter is released, the undo-buffer of the editor
       is flushed unless CreateUndoableWriter was called to obtain the
       IOTAEditWriter.
     o To insert Unicode text using the "Insert" method, the "Text" parameter
       should be UTF8-encoded.

    WARNING!!!
     o A IOTAEditWriter should never be active at the same time as an IOTAEditReader.
  }

  IOTAEditWriter = interface(IUnknown)
    ['{26EB0E50-F97B-11D1-AB27-00C04FB16FB3}']
    procedure CopyTo(Pos: Longint);
    procedure DeleteTo(Pos: Longint);
    procedure Insert(Text: PAnsiChar);
    function Position: Longint;
    function GetCurrentPos: TOTACharPos;

    property CurrentPos: TOTACharPos read GetCurrentPos;
  end;

  { TOTASyntaxHighlighter is deprecated }
  TOTASyntaxHighlighter = (shNone = 0,
                           shQuery = 1,
                           shPascal = 2,
                           shC = 3,
                           shSQL = 4,
                           shIDL = 5,
                           shMax = $FF);

  {
    TOTASyntaxCode:
      The values which should be used with this type are defined in ToolsAPI as:
        atWhiteSpace     = 0;
        atComment        = 1;
        atReservedWord   = 2;
        atIdentifier     = 3;
        etc...

        Do not exceed SyntaxOff
  }
  POTASyntaxCode = ^TOTASyntaxCode;
  TOTASyntaxCode = Byte;
  {
    TOTALineClass:
      The value which is used for this type in the Highlighter interfaces is
      user definable (it is used to gain context for lines).
  }
  TOTALineClass  = Byte;
  {$IFDEF FATCHAR}
  OTAEdChar = WideChar;
  POTAEdChar = PWideChar;
  {$ELSE}
  OTAEdChar = AnsiChar;
  POTAEdChar = PAnsiChar;
  {$ENDIF}
  {
    TOTALineSize:
      Length of line buffer to be tokenized.
  }
  TOTALineSize = Word;

  IOTAHighlighter = interface(IOTANotifier)
    ['{79D28DA1-42F6-44B9-AE33-D001FD75DC40}']
    function GetIDString: string;
    function GetName: string;
    procedure Tokenize(StartClass: TOTALineClass; LineBuf: POTAEdChar;
      LineBufLen: TOTALineSize; HighlightCodes: POTASyntaxCode);
    function TokenizeLineClass(StartClass: TOTALineClass;
      LineBuf: POTAEdChar; LineBufLen: TOTALineSize): TOTALineClass;
    property Name: string read GetName;
    property IDString: string read GetIDString;
  end;

  { If an IOTAHighlighter implements IOTAHighlighterPreview, a
    preview of that highlighter's code will be shown in the color page. }
  IOTAHighlighterPreview = interface
    ['{86A7ABCC-F81C-479D-B25D-C105F0DF1254}']
    { The display name for the source sample }
    function GetDisplayName: string;
    { SampleText to highlight }
    function GetSampleText: string;
    { Retrieve the line for current settings. Return -1 to not show it. }
    function GetInvalidBreakpointLine: Integer;
    function GetCurrentInstructionLine: Integer;
    function GetValidBreakpointLine: Integer;
    function GetDisabledBreakpointLine: Integer;
    function GetErrorLine: Integer;
    { Sample text to search for in order to show it as highlighted.
      Return an empty string to not support it }
    function GetSampleSearchText: string;
    { Block start/end positions MUST be valid. 1-based indexes. }
    function GetBlockStartLine: Integer;
    function GetBlockStartCol: Integer;
    function GetBlockEndLine: Integer;
    function GetBlockEndCol: Integer;
  end;

  { If implemented by the personality, used to specify the default
    highlighter to show. }
  IOTADefaultPreviewTrait = interface
    ['{0BFB4FE6-77F3-43AE-97A9-F740F8816126}']
    { Return the IDString for the highlighter to default to for a preview. }
    function GetDefaultHighlighterPreview: string;
    property DefaultHighlighterPreview: string read GetDefaultHighlighterPreview;
  end;

{ If an IOTAHighlighter implements IOTAElisionPreview in addition to
  IOTAHighlighterPreview, a preview of that highlighter's elision points
  will be shown in the color page. A sanity check is performed on the
  values returned from these methods to ensure 1) that the End postiion
  is after the Start position and 2) that the elision blocks do not go
  beyond the end of the code sample.  Elisions that do not pass this
  sanity check are ignored.  You can use this, for example, if you only
  want to show an elidable block without also showing an elided block.
  To do that, return -1 from the *Elided* methods below.
  The Descriptions can be blank.  In that case, no text is shown when
  the blocks are elided. }

  IOTAElisionPreview = interface
    ['{5637D9D8-9310-4D46-B40D-1DFEB15677D2}']
    function GetElidableBlockStartLine: Integer;
    function GetElidableBlockStartCol: Integer;
    function GetElidableBlockEndLine: Integer;
    function GetElidableBlockEndCol: Integer;
    function GetElidedBlockStartLine: Integer;
    function GetElidedBlockStartCol: Integer;
    function GetElidedBlockEndLine: Integer;
    function GetElidedBlockEndCol: Integer;
    function GetElidedBlockDescription: String;
    function GetElidableBlockDescription: String;
  end;

  IOTAHighlightServices = interface
    ['{78C26089-6CAD-40D1-BAC2-37A84DF8F3E6}']
    function GetHighlighterCount: Integer;
    function GetHighlighter(Index: Integer): IOTAHighlighter;
    function AddHighlighter(const AHighlighter: IOTAHighlighter): Integer;
    procedure RemoveHighlighter(Index: Integer);
    property Highlighter[Index: Integer]: IOTAHighlighter read GetHighlighter;
    property HighlighterCount: Integer read GetHighlighterCount;
  end;

  IOTACustomEditView = interface(IInterface)
    ['{845EDB0E-D107-4748-8444-707C680D8A65}']
    function SameView(const EditView: IOTACustomEditView): Boolean; overload;
  end;

  TOTABlockType = (btInclusive, btLine, btColumn, btNonInclusive, btUnknown);

  IOTAEditView40 = interface(IInterface)
    ['{0E3B9D78-E119-11D1-AB0C-00C04FB16FB3}']
    { Return the Current cursor position }
    function GetCursorPos: TOTAEditPos;
    { Set the current Cursor position }
    procedure SetCursorPos(const Value: TOTAEditPos);
    { Return the Current top visible position }
    function GetTopPos: TOTAEditPos;
    { Set the current top visible position }
    procedure SetTopPos(const Value: TOTAEditPos);
    { Get size of the visible portion of the view in character cells }
    function GetViewSize: TSize;
    { Converts a linear buffer offset position to a CharPos }
    function PosToCharPos(Pos: Longint): TOTACharPos;
    { Convert a CharPos to a linear buffer offset }
    function CharPosToPos(CharPos: TOTACharPos): Longint;
    { Convert between a EdPos and a CharPos }
    procedure ConvertPos(EdPosToCharPos: Boolean; var EditPos: TOTAEditPos;
      var CharPos: TOTACharPos);
    { Return the token attributes at the given EdPos. If IncludeMargin is true,
      the attribute at the right margin line is the margin line attribute, else
      it returns the actual char attribute }
    procedure GetAttributeAtPos(const EdPos: TOTAEditPos; IncludeMargin: Boolean;
      var Element, LineFlag: Integer);
    { Returns true if this interface instance is connected to the same
      underlying view as the indicated instance.  You must use this method
      in order to test for equality between views since several interface
      instances may share a single view.  You should also not hold onto
      these view interfaces for any length of time, unless a notifier is
      registered with the IOTASourceEditor so you can determine when the
      underlying implementation is vanishing. }
    function SameView(const EditView: IOTAEditView): Boolean;

    property CursorPos: TOTAEditPos read GetCursorPos write SetCursorPos;
    property TopPos: TOTAEditPos read GetTopPos write SetTopPos;
    property ViewSize: TSize read GetViewSize;
  end;

  IOTASearchOptions = interface(IUnknown)
    ['{D1766F8B-D915-11D2-A8C1-00C04FA32F53}']
    function GetCaseSensitive: Boolean;
    function GetDirection: TSearchDirection;
    function GetFromCursor: Boolean;
    function GetRegularExpression: Boolean;
    function GetSearchText: string;
    function GetWholeFile: Boolean;
    function GetWordBoundary: Boolean;
    procedure SetCaseSensitive(Value: Boolean);
    procedure SetDirection(Value: TSearchDirection);
    procedure SetFromCursor(Value: Boolean);
    procedure SetRegularExpression(Value: Boolean);
    procedure SetSearchText(const Value: string);
    procedure SetWholeFile(Value: Boolean);
    procedure SetWordBoundary(Value: Boolean);

    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
    property Direction: TSearchDirection read GetDirection write SetDirection;
    property FromCursor: Boolean read GetFromCursor write SetFromCursor;
    property RegularExpression: Boolean read GetRegularExpression write SetRegularExpression;
    property SearchText: string read GetSearchText write SetSearchText;
    property WholeFile: Boolean read GetWholeFile write SetWholeFile;
    property WordBoundary: Boolean read GetWordBoundary write SetWordBoundary;
  end;

  IOTAReplaceOptions = interface(IOTASearchOptions)
    ['{D1766F8C-D915-11D2-A8C1-00C04FA32F53}']
    function GetPromptOnReplace: Boolean;
    function GetReplaceAll: Boolean;
    function GetReplaceText: string;
    procedure SetPromptOnReplace(Value: Boolean);
    procedure SetReplaceAll(Value: Boolean);
    procedure SetReplaceText(const Value: string);

    property PromptOnReplace: Boolean read GetPromptOnReplace write SetPromptOnReplace;
    property ReplaceAll: Boolean read GetReplaceAll write SetReplaceAll;
    property ReplaceText: string read GetReplaceText write SetReplaceText;
  end;

  IOTAEditPosition = interface(IUnknown)
    ['{9C510464-C7BC-11D2-9AEB-00A02457621F}']
    procedure Align(Magnitude: Integer);
    function BackspaceDelete(HowMany: Integer): Boolean;
    function Delete(HowMany: Integer): Boolean;
    function DistanceToTab(Direction: TSearchDirection): Integer;
    function GetCharacter: Char;
    function GetColumn: Integer;
    function GetIsSpecialCharacter: Boolean;
    function GetIsWhitespace: Boolean;
    function GetIsWordCharacter: Boolean;
    function GetLastRow: Integer;
    function GetReplaceOptions: IOTAReplaceOptions;
    function GetRow: Integer;
    function GetSearchErrorString(ErrorCode: Integer): string;
    function GetSearchOptions: IOTASearchOptions;
    function GotoLine(LineNumber: Integer): Boolean;
    procedure InsertBlock(const Block: IOTAEditBlock);
    procedure InsertCharacter(Character: Char);
    procedure InsertFile(const FileName: string);
    procedure InsertText(const Text: string);
    function Move(Row, Col: Integer): Boolean;
    function MoveBOL: Boolean;
    function MoveCursor(MoveMask: TMoveCursorMasks): Boolean;
    function MoveEOF: Boolean;
    function MoveEOL: Boolean;
    function MoveReal(Row, Col: Integer): Boolean;
    function MoveRelative(Row, Col: Integer): Boolean;
    procedure Paste;
    function Read(NumberOfCharacters: Integer): string;
    function RepeatLastSearchOrReplace: Boolean;
    function Replace(const Pattern, ReplaceText: string; CaseSensitive,
      RegularExpression, WholeFile: Boolean; Direction: TSearchDirection;
      var ErrorCode: Integer): Integer; overload;
     
    function Replace: Integer; overload;
     
    function ReplaceAgain: Integer;
    procedure Restore;
    function RipText(const ValidChars: TSysCharSet; RipFlags: Integer): string; overload;

    function RipText(const ValidChars: string; RipFlags: Integer): string; overload;
     
    procedure Save;
    function Search(const Pattern: string; CaseSensitive, RegularExpression,
      WholeFile: Boolean; Direction: TSearchDirection;
      var ErrorCode: Integer): Boolean; overload;
     
    function Search: Boolean; overload;
     
    function SearchAgain: Boolean;
    procedure Tab(Magnitude: Integer);

    property Character: Char read GetCharacter;
    property Column: Integer read GetColumn;
    property IsSpecialCharacter: Boolean read GetIsSpecialCharacter;
    property IsWhiteSpace: Boolean read GetIsWhiteSpace;
    property IsWordCharacter: Boolean read GetIsWordCharacter;
    property LastRow: Integer read GetLastRow;
    property ReplaceOptions: IOTAReplaceOptions read GetReplaceOptions;
    property Row: Integer read GetRow;
    property SearchOptions: IOTASearchOptions read GetSearchOptions;
  end;

  IOTAInsertWideChar = interface(IUnknown)
    ['{A43AC8A9-7641-427B-A5F2-F3DFE937E974}']
    procedure InsertWideCharacter(Character: WideChar);
  end;

  TOTASyncMode = (smNone, smNormal, smTemplates);

  IOTASyncEditPoint100 = interface
    ['{59492E58-8CE3-45D8-902F-D80475DEFE9D}']
    procedure AddOffset(Offset: TOTACharPos);
    procedure RemoveOffset(Index: Integer);
    function GetCount: Integer;
    function GetEditable: Boolean;
    function GetHint: string;
    function GetName: string;
    function GetOffset(Index: Integer): TOTACharPos;
    function GetText: string;
    procedure SetEditable(Value: Boolean);
    procedure SetHint(const Value: string);
    procedure SetName(const Value: string);
    procedure SetOffset(Index: Integer; Value: TOTACharPos);
    procedure SetText(Value: string);

    property Count: Integer read GetCount;
    property Editable: Boolean read GetEditable write SetEditable;
    property Hint: string read GetHint write SetHint;
    property Name: string read GetName write SetName;
    property Offset[Index: Integer]: TOTACharPos read GetOffset write SetOffset;
    property Text: string read GetText write SetText;
  end;

  IOTASyncEditPoint = interface(IOTASyncEditPoint100)
    ['{B724144B-D292-4BAB-925F-BE84D7E7507C}']
    function GetMultiLine: Boolean;
    procedure SetMultiLine(Value: Boolean);
    property MultiLine: Boolean read GetMultiLine write SetMultiLine;
  end;

  IOTASyncEditPoints = interface
    ['{5E7AD63A-CB78-4BF4-9F1D-B2576898DCB4}']
    function AddPoint(const APoint: IOTASyncEditPoint): Integer;
    procedure RemovePoint(const APoint: IOTASyncEditPoint);
    function GetPoints(Index: Integer): IOTASyncEditPoint;
    function GetCount: Integer;
    property Count: Integer read GetCount;
    property Points[Index: Integer]: IOTASyncEditPoint read GetPoints; default;
  end;

  TOTASyncEditPointEventType = (sepEnter, sepLeave, sepExit);

  IOTASyncEditNotifier = interface
    ['{FA65E734-7F38-48F2-9A9C-B09650159E34}']
    procedure OnPoint(const APoint: IOTASyncEditPoint;
      const APoints: IOTASyncEditPoints;
      EventType: TOTASyncEditPointEventType);
    procedure OnSyncEdit(const APoints: IOTASyncEditPoints;
      EventType: TOTASyncEditPointEventType);
  end;

  IOTAEditBlock90 = interface(IUnknown)
    ['{9C510463-C7BC-11D2-9AEB-00A02457621F}']
    procedure BeginBlock;
    procedure Copy(Append: Boolean);
    procedure Cut(Append: Boolean);
    function Delete: Boolean;
    procedure EndBlock;
    function Extend(NewRow, NewCol: Integer): Boolean;
    function ExtendPageUp: Boolean;
    function ExtendPageDown: Boolean;
    function ExtendReal(NewRow, NewCol: Integer): Boolean;
    function ExtendRelative(DeltaRow, DeltaCol: Integer): Boolean;
    function GetEndingColumn: Integer;
    function GetEndingRow: Integer;
    function GetIsValid: Boolean;
    function GetSize: Integer;
    function GetStartingColumn: Integer;
    function GetStartingRow: Integer;
    function GetStyle: TOTABlockType;
    function GetText: string;
    function GetVisible: Boolean;
    procedure Indent(Magnitude: Integer);
    procedure LowerCase;
    function Print: Boolean;
    procedure Reset;
    procedure Restore;
    procedure Save;
    function SaveToFile(const FileName: string): Boolean;
    procedure SetStyle(Value: TOTABlockType);
    procedure SetVisible(Value: Boolean);
    procedure ToggleCase;
    procedure UpperCase;

    property EndingColumn: Integer read GetEndingColumn;
    property EndingRow: Integer read GetEndingRow;
    property IsValid: Boolean read GetIsValid;
    property Size: Integer read GetSize;
    property StartingColumn: Integer read GetStartingColumn;
    property StartingRow: Integer read GetStartingRow;
    property Style: TOTABlockType read GetStyle write SetStyle;
    property Text: string read GetText;
    property Visible: Boolean read GetVisible write SetVisible;
  end;

  IOTAEditBlock = interface(IOTAEditBlock90)
    ['{371F9A07-94E2-4708-9DB0-93514FD2FE14}']
    function GetSyncMode: TOTASyncMode;
    procedure SyncEditBlock(const Points: IOTASyncEditPoints = nil);
    function AddNotifier(const ANotifier: IOTASyncEditNotifier): Integer;
    procedure RemoveNotifier(Index: Integer);
    property SyncMode: TOTASyncMode read GetSyncMode;
  end;

  INTAEditWindow = interface(IUnknown)
    ['{8CC6430B-E721-11D2-A8CC-00C04FA32F53}']
    function GetForm: TCustomForm;
    function GetStatusBar: TStatusBar;
    function CreateDockableForm(const FormName: string): TDockableForm;
    procedure ShowDockableFormFrame(const FormName, Caption: string; AFrame: TFrame);

    property Form: TCustomForm read GetForm;
    property StatusBar: TStatusBar read GetStatusBar;
  end;

  INTAEditServicesNotifier = interface(IOTANotifier)
    ['{F954AA40-23E7-412C-BD59-CC3428DE2939}']
    procedure WindowShow(const EditWindow: INTAEditWindow; Show, LoadedFromDesktop: Boolean);
    procedure WindowNotification(const EditWindow: INTAEditWindow; Operation: TOperation);
    procedure WindowActivated(const EditWindow: INTAEditWindow);
    procedure WindowCommand(const EditWindow: INTAEditWindow; Command, Param: Integer; var Handled: Boolean);
    procedure EditorViewActivated(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
    procedure EditorViewModified(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
    procedure DockFormVisibleChanged(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
    procedure DockFormUpdated(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
    procedure DockFormRefresh(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
  end;

  {$IFDEF LINUX}
  {$NODEFINE INTAEditWindow}
  {$ENDIF}

  IOTAEditView140 = interface(IOTAEditView40)
    ['{9C510462-C7BC-11D2-9AEB-00A02457621F}']
    function BookmarkGoto(BookmarkID: Integer): Boolean;
    function BookmarkRecord(BookmarkID: Integer): Boolean;
    function BookmarkToggle(BookmarkID: Integer): Boolean;
    procedure Center(Row, Col: Integer);
    function GetBlock: IOTAEditBlock;
    function GetBookmarkPos(BookmarkID: Integer): TOTACharPos;
    function GetBottomRow: Integer;
    function GetBuffer: IOTAEditBuffer;
    function GetEditWindow: INTAEditWindow;
    function GetLastEditColumn: Integer;
    function GetLastEditRow: Integer;
    function GetLeftColumn: Integer;
    function GetPosition: IOTAEditPosition;
    function GetRightColumn: Integer;
    function GetTopRow: Integer;
    procedure MoveCursorToView;
    procedure MoveViewToCursor;
    procedure PageDown;
    procedure PageUp;
    procedure Paint;
    function Scroll(DeltaRow: Integer; DeltaCol: Integer): Integer;
    procedure SetTopLeft(TopRow, LeftCol: Integer);
    procedure SetTempMsg(const Msg: string);

    property Block: IOTAEditBlock read GetBlock;
    property BottomRow: Integer read GetBottomRow;
    property BookmarkPos[BookMarkID: Integer]: TOTACharPos read GetBookmarkPos;
    property Buffer: IOTAEditBuffer read GetBuffer;
    property LastEditColumn: Integer read GetLastEditColumn;
    property LastEditRow: Integer read GetLastEditRow;
    property LeftColumn: Integer read GetLeftColumn;
    property Position: IOTAEditPosition read GetPosition;
    property RightColumn: Integer read GetRightColumn;
    property TopRow: Integer read GetTopRow;
  end;

  IOTAEditView = interface(IOTAEditView140)
    ['{5EBBA00C-059D-4571-B30A-328D1264BBB8}']
    procedure ClearAllBookmarks;
  end;

  TClassNavigateStyle = Byte;
  TCodeCompleteStyle = Byte;
  TOTANavigateType = (ntUp, ntDown, ntHome, ntEnd);

  IOTAEditActions60 = interface(IUnknown)
    ['{ECB23623-E2D8-11D2-ABE5-00C04FB16FB3}']
    procedure AddWatch;
    procedure AddWatchAtCursor;
    procedure BrowseSymbolAtCursor;
    procedure ClassComplete;
    procedure ClassNavigate(Reserved: TClassNavigateStyle);
    procedure ClosePage;
    procedure CodeTemplate;
    procedure CodeCompletion(Style: TCodeCompleteStyle);
    procedure EvaluateModify;
    procedure HelpKeyword;
    procedure IncrementalSearch;
    procedure InsertCompilerOptions;
    procedure InsertNewGUID;
    procedure InspectAtCursor;
    procedure CompileProject;
    procedure NextError;
    procedure NextPage;
    procedure OpenFile;
    procedure OpenFileAtCursor;
    procedure PriorError;
    procedure PriorPage;
    procedure ProgramReset;
    procedure RunProgram;
    procedure RunToCursor;
    procedure SaveAll;
    procedure Save;
    procedure SaveAs;
    procedure StepOver;
    procedure SwapSourceFormView;
    procedure SwapCPPHeader;
    procedure ToggleFormUnit;
    procedure TraceInto;
    procedure TraceToSource;
    procedure ViewExplorer;
    procedure ViewForms;
    procedure ViewObjectInspector;
    procedure ViewUnits;
    procedure WindowList;
    procedure ZoomWindow;
  end;

  { IOTAEditView implements IOTAEditActions }
  IOTAEditActions100 = interface(IOTAEditActions60)
    ['{9A073F11-2732-D611-958B-00C04FA06AFC}']
    procedure NextBufferView;
    procedure PreviousBufferView;
  end;

  { IOTAEditView implements IOTAEditActions }
  IOTAEditActions = interface(IOTAEditActions100)
    ['{332DBE22-AB36-44B7-B835-BC95F6F9E688}']
    procedure MethodNavigate(NavigateType: TOTANavigateType);
  end;

  { IOTAEditView implements IOTAElideActions }
  IOTAElideActions120 = interface(IUnknown)
    ['{3AB41D14-D290-4674-8978-D0AB4E70C14B}']
    { Elide (fold) the nearest block }
    procedure ElideNearestBlock;
    { Unelide (unfold) the nearest block }
    procedure UnElideNearestBlock;
    { Unelide all blocks }
    procedure UnElideAllBlocks;
    { Enable elisions (folding) }
    procedure EnableElisions;
  end;

  { IOTAEditView implements IOTAElideActions }
  IOTAElideActions = interface(IOTAElideActions120)
    ['{8080361F-888F-4D0E-9ECF-3FE21FA768B5}']
    { Toggle elisions (folding) of nearest block }
    procedure ToggleElisions;
    { Elide (fold) all Namespaces }
    procedure ElideNamespaces;
    { Elide (fold) all Regions }
    procedure ElideRegions;
    { Elide (fold) all Types }
    procedure ElideTypes;
    { Elide (fold) all Nested Procedures }
    procedure ElideNestedProcs;
    { Elide (fold) all Globals }
    procedure ElideGlobals;
    { Elide (fold) all Methods }
    procedure ElideMethods;
  end;

  IOTASourceEditor70 = interface(IOTAEditor)
    ['{F17A7BD1-E07D-11D1-AB0B-00C04FB16FB3}']
    { Create and return an IOTAEditReader }
    function CreateReader: IOTAEditReader;
    { Create and return an IOTAEditWriter. Changes are not undoable }
    function CreateWriter: IOTAEditWriter;
    { Create and return an IOTAEditWriter. Changes are undoable }
    function CreateUndoableWriter: IOTAEditWriter;
    { Return the number of active views on this editor }
    function GetEditViewCount: Integer;
    { Return the Indexed view }
    function GetEditView(Index: Integer): IOTAEditView;
    { Returns the total number of lines in this source editor }
    function GetLinesInBuffer: Longint;
    { Change the syntax highlighter for this buffer or if shQuery is set,
      simply return the currently set highlighter.
      SetSyntaxHighlighter is deprecated. Use the IOTAEditOptions. }
    function SetSyntaxHighlighter(SyntaxHighlighter: TOTASyntaxHighlighter): TOTASyntaxHighlighter; deprecated;
    { These functions will affect all views on this buffer. }
    function GetBlockAfter: TOTACharPos;
    function GetBlockStart: TOTACharPos;
    function GetBlockType: TOTABlockType;
    function GetBlockVisible: Boolean;
    procedure SetBlockAfter(const Value: TOTACharPos);
    procedure SetBlockStart(const Value: TOTACharPos);
    procedure SetBlockType(Value: TOTABlockType);
    procedure SetBlockVisible(Value: Boolean);

    property BlockStart: TOTACharPos read GetBlockStart write SetBlockStart;
    property BlockAfter: TOTACharPos read GetBlockAfter write SetBlockAfter;
    property BlockType: TOTABlockType read GetBlockType write SetBlockType;
    property BlockVisible: Boolean read GetBlockVisible write SetBlockVisible;
    property EditViewCount: Integer read GetEditViewCount;
    property EditViews[Index: Integer]: IOTAEditView read GetEditView;
  end;

  IOTASourceEditor = interface(IOTASourceEditor70)
    ['{5D965803-8147-4D32-B8C5-712F6DDCF98E}']
    { Get the number of sub-Views on this editor.  This editor itself may be a
      sub view and may not be at index 0.  !!NOTE!! that this function will
      return 0 if this SourceEditor is not visible in any editor window.  You must
      call IOTAEditor.Show before using these functions to manipulate the views. }
    function GetSubViewCount: Integer;
    { Returns a view identifier for the given sub-view index.  This may just be
      the specific filename of the view or some other unique identifier }
    function GetSubViewIdentifier(Index: Integer): string;
    { Returns the sub-view index for this editor.  If this is the main source
      editor, its index will always be 0.  However for modules with more than
      one source editor, this may return > 0. (ie. Managed .cpp files with an
      associated .h file) }
    function GetSubViewIndex: Integer;
    { Switches the editor to the specified view by view index }
    procedure SwitchToView(Index: Integer); overload;
    { Switches the editor to the specified view by view identifier }
    procedure SwitchToView(const AViewIdentifier: string); overload;
  end;

  { IOTAResourceEntry is a raw interface to a resource entry in the project's
    resource file (<projectname>.RES).

    This interface is very raw.  No implication on what is contained within
    a particular entry is made.  Is if up to the add-in developer to interpret
    the data accessed through this interface.  NOTE: The 'MAINICON' entry and
    related entries should not be modified as these are maintained by Delphi/
    C++Builder. }

  TOTAResHeaderValue = (hvFlags, hvLanguage, hvDataVersion, hvVersion,
    hvCharacteristics);

  IOTAResourceEntry = interface(IUnknown)
    ['{26EB0E51-F97B-11D1-AB27-00C04FB16FB3}']
    { Gets the resource type of this entry.  Follows Windows standard of
      specifying a type by name or value.  If the high-word is 0, then the
      low-word is the resource type value, otherwise it is a pointer to a null
      terminated ANSI (byte per char) string. Most predefined types are by
      value. }
    function GetResourceType: PChar;
    { Gets the resource name of this entry.  Follows Windows standard of
      specifying a type by name or value. If the high-word is 0, then the
      low-word is the resource type value, otherwise it is a pointer to a null
      terminated ANSI (byte per char) string. }
    function GetResourceName: PChar;
    { Changes the Type and name of this resource entry }
    function Change(NewType, NewName: PChar): Boolean;
    { Gets and sets various resource header values.  Pass in one of the \
      TResHeaderValues enums to indicate which value to get/set.  Although
      some values are 16bits (Word) these functions operation only on
      32bits (Integer). }
    function GetHeaderValue(HeaderValue: TOTAResHeaderValue;
      var Value: Integer): Boolean;
    { See GetHeaderValue }
    function SetHeaderValue(HeaderValue: TOTAResHeaderValue;
      Value: Integer): Boolean;
    { Returns a raw pointer to the actual resource data buffer. }
    function GetData: Pointer;
    { Returns the current size of the data buffer. }
    function GetDataSize: Integer;
    { Resizes the current data buffer.  If the size is smaller than the
      current size, the data is simply truncated without regard to its
      current contents. }
    procedure SetDataSize(NewSize: Integer);
    { Returns a unique handle value identifying the resource entry. }
    function GetEntryHandle: TOTAHandle;

    property DataSize: Integer read GetDataSize write SetDataSize;
  end;

  { The IOTAProjectResource is an interface on the project's resource file
    (<projectname>.RES). }

  IOTAProjectResource = interface(IOTAEditor)
    ['{26EB0E52-F97B-11D1-AB27-00C04FB16FB3}']
    { Returns the number of Resource entries. }
    function GetEntryCount: Integer;
    { Given an index, returns a IOTAResourceEntry of the index'th entry. }
    function GetEntry(Index: Integer): IOTAResourceEntry;
    { Given an entry handle, return the IOTAResourceEntry }
    function GetEntryFromHandle(EntryHandle: TOTAHandle): IOTAResourceEntry;
    { Given a Resource type and name, return a IOTAResourceEntry or nil
      if not found. }
    function FindEntry(ResType, Name: PChar): IOTAResourceEntry;
    { Given an entry handle, delete the given resource entry. }
    procedure DeleteEntry(EntryHandle: TOTAHandle);
    { Creates a new resource entry of the given type and name and returns a
      IOTAResourceEntry.  Returns nil if the entry already exists or any other
      error occurs. }
    function CreateEntry(ResType, Name: PChar; Flags, LanguageId: Word;
      DataVersion, Version, Characteristics: Integer): IOTAResourceEntry;
  end;

  TOTAGetChildCallback = procedure (Param: Pointer; Component: IOTAComponent;
    var Result: Boolean) of object;

  { INTAComponent - This is the native component interface.  You can get
    this interface by checking if IOTAComponent supports it. }

  INTAComponent = interface(IUnknown)
    ['{34B2E2D1-E36F-11D1-AB0E-00C04FB16FB3}']
    { Returns the actual TComponent/TPersistent }
    function GetPersistent: TPersistent;
    { Returns the TComponent if this interface is a TComponent else nil }
    function GetComponent: TComponent;
  end;
  {$IFDEF LINUX}
  {$NODEFINE INTAComponent}
  {$ENDIF}

  { The IOTAComponent is the base interface for a component living
    on a form/data module.  Never hold this interface for very long, since
    the component may be deleted at any time. }

  IOTAComponent = interface(IUnknown)
    ['{AC139ADF-329A-D411-87C6-9B2730412200}']
    // Old GUID ['{34B2E2D0-E36F-11D1-AB0E-00C04FB16FB3}']
    { Returns a string representing the type of the component. }
    function GetComponentType: string;
    { Returns a unique Handle to the TComponent/TPersistent }
    function GetComponentHandle: TOTAHandle;
    { Returns the interface corresponding to the parent control if a TControl,
      otherwise returns the owner of the control.  If a TPersistent or the
      root object then it returns nil. }
    function GetParent: IOTAComponent;
    { Returns True if component is a TControl descendant }
    function IsTControl: Boolean;
    { Returns the number of published properties on this component. }
    function GetPropCount: Integer;
    { Given the index, returns the property name. }
    function GetPropName(Index: Integer): string;
    { Given the index, returns the property type. }
    function GetPropType(Index: Integer): TTypeKind;
    { Given the name, returns the property type. }
    function GetPropTypeByName(const Name: string): TTypeKind;
    { Given the index or name, returns the property value. The untyped var
      must be large enough to hold the returned value.  For string types,
      the untyped var must match the actual string type (as indicated by GetPropType
      or GetPropTypeByName):
        -- tkString:  untyped param should be of type ShortString
        -- tkLString: untyped param should be of type AnsiString
        -- tkWString: untyped param should be of type WideString
        -- tkUString: untyped param should be of type UnicodeString
      If the property is a descendant of TPersistent, the return value is a
      IOTAComponent. For properties of any other objecttype, the return value is nil. }
    function GetPropValue(Index: Integer; var Value): Boolean;
    function GetPropValueByName(const Name: string; var Value): Boolean;
    { Given the index or name, sets the property value. }
    function SetProp(Index: Integer; const Value): Boolean;
    function SetPropByName(const Name: string; const Value): Boolean;
    { Enumerate the child controls just like TComponent.GetChildren }
    function GetChildren(Param: Pointer; Proc: TOTAGetChildCallback): Boolean;
    { Returns the number of child controls (if a TWinControl/TWidgetControl descendant,
      else returns 0). }
    function GetControlCount: Integer;
    { Given the index, returns an interface to the child control. }
    function GetControl(Index: Integer): IOTAComponent;
    { Returns the number of child components (if a TComponent descendant,
      else returns 0). }
    function GetComponentCount: Integer;
    { Given the index, returns an interface to the child component. }
    function GetComponent(Index: Integer): IOTAComponent;
    { Selects the component and updates the Object Inspector. If AddToSelection
      if true, then the current selection is not cleared, and the components are
      multi-selected }
    function Select(AddToSelection: Boolean): Boolean;
    { Same as Select except it brings the form to front with the component
      selected.  If this interface is a Form/Data Module, then Focus only
      brings the form to front. See Select for description of AddToSelection}
    function Focus(AddToSelection: Boolean): Boolean;
    { Deletes the component from the form.  Following this call, this interface
      will now be invalid and must be release. }
    function Delete: Boolean;
    { Returns the IPersistent interface }
    //function GetIPersistent: IPersistent;
    { Returns the IComponent interface if instance is a TComponent else nil }
    //function GetIComponent: IComponent;
  end;

  { IOTAFormEditor implements INTAFormEditor }
  INTAFormEditor = interface(IUnknown)
    ['{56931EB9-329A-D411-87C6-9B2730412200}']
    // Old GUID ['{34B2E2CF-E36F-11D1-AB0E-00C04FB16FB3}']
    { Return the instance of the TFormDesigner on this editor }
    function GetFormDesigner: uDesignIntf.IDesigner;
    procedure GetFormResource(Stream: TStream);

    property FormDesigner: uDesignIntf.IDesigner read GetFormDesigner;
  end;
  {$IFDEF LINUX}
  {$NODEFINE INTAFormEditor}
  {$ENDIF}

  IOTAFormEditor = interface(IOTAEditor)
    ['{F17A7BD2-E07D-11D1-AB0B-00C04FB16FB3}']
    { Return the form editor root component }
    function GetRootComponent: IOTAComponent;
    function FindComponent(const Name: string): IOTAComponent;
    function GetComponentFromHandle(ComponentHandle: TOTAHandle): IOTAComponent;
    function GetSelCount: Integer;
    function GetSelComponent(Index: Integer): IOTAComponent;
    function GetCreateParent: IOTAComponent;
    function CreateComponent(const Container: IOTAComponent;
      const TypeName: string; X, Y, W, H: Integer): IOTAComponent;
    procedure GetFormResource(const Stream: IStream);
  end;

  IOTATypeLibrary = interface
    ['{7A2F5910-58D2-448E-B457-2DC01E853D46}']
  end;

  IOTATypeLibEditor = interface(IOTAEditor)
    ['{F17A7BD3-E07D-11D1-AB0B-00C04FB16FB3}']
    function GetTypeLibrary: IOTATypeLibrary;

    property TypeLibrary: IOTATypeLibrary read GetTypeLibrary;
  end;

  { Interface implemented by a client to receive notifications
    on a specific module }
  IOTAModuleNotifier = interface(IOTANotifier)
    ['{F17A7BCE-E07D-11D1-AB0B-00C04FB16FB3}']
    { CheckOverwrite is called during a SaveAs operation to determine if any
      files associated with this module will overwrite any other files.
      Return True to allow the overwrite or no overwrite will occur }
    function CheckOverwrite: Boolean;
    { User has renamed the module }
    procedure ModuleRenamed(const NewName: string);
  end;

  IOTAModuleNotifier80 = interface(IOTAModuleNotifier)
    ['{6C4714BB-223A-4CDF-A710-429FE8FA0B91}']
    { AllowSave is called immediately prior to doing any type of save operation
      in order to allow any add-ins to enable/disable the saving of any specific
      module.  This is useful when one module is to be kept in sync with another
      module such as keeping the name of a module the same base name as the
      project. }
    function AllowSave: Boolean;
    { GetOverwriteFileNameCount returns the number of filenames to check for an
      overwrite during a save as operation.  This is simply a list of files that
      the IDE will check if they exist.  If any of these files exist, then the
      IDE will prompt for an overwrite and display the filename in the overwrite
      prompt dialog. }
    function GetOverwriteFileNameCount: Integer;
    { GetOverwriteFileName returns the index'd filename for the IDE to check for
      existence during a save as operation. }
    function GetOverwriteFileName(Index: Integer): string;
    { SetSaveFileName will be called with the fully qualified filename that the
      user entered in the Save As dialog.  This name can then be used to
      determine all the resulting names }
    procedure SetSaveFileName(const FileName: string);

    property OverwriteFileNameCount: Integer read GetOverwriteFileNameCount;
    property OverwriteFileNames[Index: Integer]: string read GetOverwriteFileName;
  end;

  IOTAModuleNotifier90 = interface(IOTAModuleNotifier80)
    ['{53B90D0D-0E70-472A-885E-0566B2711C15}']
    { BeforeRename is call just before the new file is save/renamed on disk.}
    procedure BeforeRename(const OldFileName, NewFileName: string);
    { AfterRename is call just after the new file is save/renamed on disk.}
    procedure AfterRename(const OldFileName, NewFileName: string);
  end;

  IOTAModuleInfo50 = interface(IUnknown)
    ['{F17A7BD6-E07D-11D1-AB0B-00C04FB16FB3}']
    { Returns the type of this module }
    function GetModuleType: TOTAModuleType;
    { Returns the Module Name }
    function GetName: string;
    { Returns the Module File name }
    function GetFileName: string;
    { Returns the Form Name }
    function GetFormName: string;
    { Returns the Design class }
    function GetDesignClass: string;
    { Fills the TStrings class with the CoClasses }
    procedure GetCoClasses(CoClasses: TStrings);
    { Opens and returns the IOTAModule associated with this IOTAModuleInfo }
    function OpenModule: IOTAModule;

    property ModuleType: TOTAModuleType read GetModuleType;
    property Name: string read GetName;
    property FileName: string read GetFileName;
    property FormName: string read GetFormName;
    property DesignClass: string read GetDesignClass;
  end;

  IOTAModuleInfo = interface(IOTAModuleInfo50)
    ['{B3EEB4D2-ECDD-4CDC-B96E-B5C8F6D050A8}']
    { Returns the Custom module type identifier }
    function GetCustomId: string;
    { Fills the TStrings class with the Additional files }
    procedure GetAdditionalFiles(Files: TStrings);

    property CustomId: string read GetCustomId;
  end;

  IOTAModule40 = interface(IUnknown)
    ['{F17A7BCC-E07D-11D1-AB0B-00C04FB16FB3}']
    { Call this to register an IOTANotifier. The result is the index to be
      used when calling RemoveNotifier. If <0 then an error occurred. }
    function AddNotifier(const ANotifier: IOTAModuleNotifier): Integer;
    { This invokes the Add To Interface dialog in Delphi }
    procedure AddToInterface;
    { Attempt to close this module. True was successful and all references to
      this module must be released. False if this module was not closed. }
    function Close: Boolean;
    { Return the filename associated with this module.  This is only the base
      name used by the IDE.  Header source and forms are obtained other ways.}
    function GetFileName: string;
    { Return the currently assigned file system }
    function GetFileSystem: string;
    { Returns the number of associated files (eg. Unit1.Pas and Unit1.dfm) }
    function GetModuleFileCount: Integer;
    { Returns the associated file editor.  Use QueryInterface to determine if
      this is an IOTASourceEditor or IOTAFormEditor }
    function GetModuleFileEditor(Index: Integer): IOTAEditor;
    { Return the number of open projects that own this module }
    function GetOwnerCount: Integer; deprecated;
    { Return the Indexed Project that owns this module }
    function GetOwner(Index: Integer): IOTAProject; deprecated;
    { Returns True if this modules has CoClasses.  Can be used to determine if
      AddToInterface can be called }
    function HasCoClasses: Boolean;
    { Call with the index obtained from AddNotifier }
    procedure RemoveNotifier(Index: Integer);
    { Save the module. ChangeName invokes the SaveAs logic.  ForceSave will not
      ask to save if the module is modified. Returns False if canceled
      or an error }
    function Save(ChangeName, ForceSave: Boolean): Boolean;
    { Sets the module filename.  Header source and forms will use the base
      filename. }
    procedure SetFileName(const AFileName: string);
    { Sets the associated file system }
    procedure SetFileSystem(const AFileSystem: string);

    property OwnerCount: Integer read GetOwnerCount;
    property Owners[Index: Integer]: IOTAProject read GetOwner;
    property FileName: string read GetFileName write SetFileName;
    property FileSystem: string read GetFileSystem write SetFileSystem;
  end;

  IOTAModule50 = interface(IOTAModule40)
    ['{15D3FB81-EF27-488E-B2B4-26B59CA89D9D}']
    { CloseModule allows an add-in to force a module closed regardless of
      whether or not it is modified.  If ForceClosed is False, then calling
      this method has the same behavior as Close as implemented in
      IOTAModule40 }
    function CloseModule(ForceClosed: Boolean): Boolean;

    // access to the IOTAEditors contained by a Module
    property ModuleFileCount: Integer read GetModuleFileCount;
    property ModuleFileEditors[Index: Integer]: IOTAEditor read GetModuleFileEditor;
  end;

  IOTAModule70 = interface(IOTAModule50)
    ['{2438BFB8-C742-48CD-8F50-DE6C7F764A55}']
    { GetCurrentEditor returns the topmost editor associated with this module.
      If the form editor was the last editor focused, then that is returned,
      likewise, if one of the code editor was the last focused then that one
      is returned. }
    function GetCurrentEditor: IOTAEditor;
    { Return the number of open modules that own this module }
    function GetOwnerModuleCount: Integer;
    { Return the Indexed module that owns this module. If this is an IOTAProject
      then these two methods may not return and owner in the case where this is
      a package project and it is not a member of the currently open project group. }
    function GetOwnerModule(Index: Integer): IOTAModule;
    { MarkModifed will mark this module as "modified" without actually indicating
      *why* is it modified (which will cause internal file dates to remain constant).
      This *will* force the IDE to ask to save this module when the user attempts
      to close it.  It will also clear the "discardability" of a new unnamed
      module such as when File|New|Application is selected. }
    procedure MarkModified;

    property CurrentEditor: IOTAEditor read GetCurrentEditor;
    property OwnerModuleCount: Integer read GetOwnerModuleCount;
    property OwnerModules[Index: Integer]: IOTAModule read GetOwnerModule;
  end;

  IOTAModule = interface(IOTAModule70)
    ['{7FF96161-E610-4414-B8B1-D1ECA76FEAFB}']
    { Call show to show the default editor for the module. }
    procedure Show;
    { Call ShowFilename to show the editor for the given filename associated
      with the module. }
    procedure ShowFilename(const FileName: string);
  end;

  TOTARegionKind = Integer;

  TOTARegion = packed record
    { RegionKind describes the type of region this is }
    RegionKind: TOTARegionKind;
    { Start is the first location of the region }
    Start: TOTACharPos;
    { Stop is the end of the regions }
    Stop: TOTACharPos;
    { Name describes the region (this is typically info gleaned from a
      syntactic element.  It may be blank. }
    Name: string;
    { Active will be true if this region will be considered by the compiler or
      whatever translator is used for the file.  This value will be True most of
      the time. }
    Active: Boolean;
  end;

  TOTARegions = array of TOTARegion;

  IOTAModuleRegions = interface(IInterface)
    ['{A7E7D74C-D123-4FAF-BE36-C4FE126B07FC}']
    { Query an IOTAModule for this interface in order to obtain source code
      region information.  This info is applied to the editor buffer to describe
      those regions in the editor that can be collapsed/expanded }
    function GetRegions(const AFileName: string = ''): TOTARegions;
  end;

  TOTAError = packed record
    { Text of the error message }
    Text: string;
    { Start is the start of the error region }
    Start: TOTACharPos;
    { Stop is the end of the error region. If Start = Stop then there is only
      a single character for the error }
    Stop: TOTACharPos;
  end;

  TOTAErrors = array of TOTAError;

  IOTAModuleErrors = interface(IInterface)
    ['{8626BF14-1526-4B88-826E-0935DB0DBB25}']
    { Query an IOTAModule for this interface in order to obtain the source
      code error information. This information is then used by the editor to
      show error hints and red underlines where the errors are located in the
      source }
    function GetErrors(const AFileName: string = ''): TOTAErrors;
  end;

  IOTAAdditionalModuleFiles = interface
    ['{2D73A12F-6FB3-11D4-A4B8-00C04F6BB853}']
    function GetAdditionalModuleFileCount: Integer;
    { Returns the associated IOTASourceEditor. }
    function GetAdditionalModuleFileEditor(Index: Integer): IOTAEditor;
    property AdditionalModuleFileCount: Integer read GetAdditionalModuleFileCount;
    property AdditionalModuleFileEditors[Index: Integer]: IOTAEditor read GetAdditionalModuleFileEditor;
  end;

  IOTAModuleData = interface
    ['{FFD0A5AF-49CB-4EC2-A658-957146030CEC}']
    function HasObjects: Boolean;
  end;

  IOTAModuleCleanup = interface
    ['{682E85BB-AF62-4868-BEFF-0C1515F006A8}']
    { When implemented on a module, this method will delete generated files like .dcu, .exe, etc }
    procedure CleanupFiles;
  end;

  IOTATypeLibModule = interface(IOTAModule)
    ['{0BBAEEA0-EF74-11D1-AB1C-00C04FB16FB3}']
    function GetTypeLibEditor: IOTATypeLibEditor;
    function GetFileName: string;
    function GetModified: Boolean;

    property TypeLibEditor: IOTATypeLibEditor read GetTypeLibEditor;
    property FileName: string read GetFileName;
    property Modified: Boolean read GetModified;
  end;

  IOTAOptions = interface(IUnknown)
    ['{9C0E91FC-FA5A-11D1-AB28-00C04FB16FB3}']
    { Opens the options dialog }
    procedure EditOptions;
    { Get the value of the named option. }
    function GetOptionValue(const ValueName: string): Variant;
    { Set the value of the named option. }
    procedure SetOptionValue(const ValueName: string; const Value: Variant);
    { Get the list of available options for this option structure }
    function GetOptionNames: TOTAOptionNameArray;

    property Values[const ValueName: string]: Variant read GetOptionValue write SetOptionValue;
  end;

  IOTAProjectOptions40 = interface(IOTAOptions)
    ['{F17A7BD4-E07D-11D1-AB0B-00C04FB16FB3}']
  end;

  { One special note about IOTAProjectOptions.
    Under Windows, there is one option called "Keys" and this options is of
    type tkClass.  This is a TStrings object and corresponds to the list of
    key/value pairs in the project's version info.  When GetOptionValue is
    called for the "Keys" Option it will be returned as a Variant varType of
    vtInteger.  You should cast the Variant result to Integer then to TStrings.
    NOTE: Do NOT free the TStrings object that is returned as this is owned by
    the internal object.  Calling SetOptionValue with a TStrings object (not
    the one read with GetOptionValue), will cause the typical "SetKeys" method
    to be called and the internal Keys object is "Assigned" the values from
    the given TStrings object (ie. FKeys.Assign(Value);) }

  IOTAProjectOptions70 = interface(IOTAProjectOptions40)
    ['{F899EBC6-E6E2-11D2-AA90-00C04FA370E9}']
    {Set the modified state of the project options}
    procedure SetModifiedState(State: Boolean);
    {Get the modified state of the project options}
    function GetModifiedState: Boolean;

    property ModifiedState: Boolean read GetModifiedState write SetModifiedState;
  end;

  IOTAProjectOptions = interface(IOTAProjectOptions70)
    ['{2888E741-E7FB-4BBC-A093-4B0903D9D990}']
    { Returns what the final output target filename and path will be for this
      project }
    function GetTargetName: string;

    property TargetName: string read GetTargetName;
  end;

  { Provides access to a Build Configuration.  For methods that take a "PropName"
    parameter, the list of valid PropNames can be found in the following source
    files:
      DCCStrs.pas:   names for Delphi compiler options
      BCCStrs.pas:   names for the C++ compiler options
      ILinkStrs.pas: names for the C++ linker options
      TLibStrs.pas:  names for the TLib tool options
      TasmStrs.pas:  names for the turbo assembler options
      BRCCStrs.pas:  names for the resource compiler options
  }
  IOTABuildConfiguration = interface(IUnknown)
    ['{92A52A72-B0D2-4898-8A20-298132F74C16}']
    { The following methods provide information about this Build Configuration }
    { Get the name of the configuration.  This may be translated }
    function GetName: string;
    { Set the name of this configuration }
    procedure SetName(const Value: string);
    { Return the configuration's unique key -- this is non-translatable }
    function GetKey: string;
    { Return the parent of this IOTABuildConfiguration. Only the "Base" configuration
      should return nil }
    function GetParent: IOTABuildConfiguration;
    { Return the number of configurations based directly on this build configuration,
      including local overrides }
    function GetChildCount: Integer;
    { Return the index'd configuration based directly on this build configuration,
      including local overrides }
    function GetChild(Index: Integer): IOTABuildConfiguration;
    { Return the number of properties specified at this configuration }
    function GetPropertyCount: Integer;
    { Return the index'd property specified at this configuration }
    function GetPropertyName(Index: Integer): string;
    { Returns whether or not the configuration has any properties at all }
    function IsEmpty: Boolean;
    { Returns whether or not the configuration has been modified since last save }
    function IsModified: Boolean;
    { Remove all properties with name "PropName" from the configuration }
    procedure Remove(const PropName: string);
    { Remove all properties }
    procedure Clear;
    { Returns true if the property "PropName" has any value, including a null
      or empty value, false if no property of that name exists in the configuration }
    function PropertyExists(const PropName: string): Boolean;

    { The following methods are getters and setters for individual properties.  Any
      property can be set/get as a string using GetValue/SetValue }
    { Retrieve the value of property "PropName" recursing through parent configurations.
      Returns empty string if the property doesn't exist. Can't tell empty properties
      from non-existent ones, use PropertyExists() to determine if a property actually is
      specified. }
    function GetValue(const PropName: string): string; overload;
    { Perform an evaluation the value of property "PropName" as a string,
      optionally recursing through parent configurations. If IncludeInheritedValues=false,
      the value is determined at this configuration only. }
    function GetValue(const PropName: string; IncludeInheritedValues: Boolean): string; overload;
    { Set the value of property "PropName" }
    procedure SetValue(const PropName, Value: string);
    { Perform a full evaluation the value of property "PropName" as a boolean,
      recursing through parent configurations to determine the value if
      none is specified on this configuration.
      Assumes empty value to be 'false' if no default value for the property is
      specified. Raises an exception if the property value cannot be
      converted to a boolean. }
    function GetBoolean(const PropName: string): Boolean; overload;
    { Perform an evaluation the value of property "PropName" as a boolean,
      optionally recursing through parent configurations. If IncludeInheritedValues=false,
      the value is determined at this configuration only.
      Assumes empty value to be 'false' if no default value for the property is
      specified. Raises an exception if the property value cannot be
      converted to a boolean. }
    function GetBoolean(const PropName: string; IncludeInheritedValues: Boolean): Boolean; overload;
    { Sets value of "PropName" as a boolean on this configuration }
    procedure SetBoolean(const PropName: string; const Value: Boolean);
    { Perform a full evaluation the value of property "PropName" as an integer,
      recursing through parent configurations to determine the value if
      none is specified on this configuration.
      Raises an exception if the property value cannot be converted to an integer. }
    function GetInteger(const PropName: string): Integer; overload;
    { Perform an evaluation the value of property "PropName" as an integer,
      optionally recursing through parent configurations. If IncludeInheritedValues=false,
      the value is determined at this configuration only.
      Assumes empty value to be 'false' if no default value for the property is
      specified. Raises an exception if the property value cannot be
      converted to an integer. }
    function GetInteger(const PropName: string; IncludeInheritedValues: Boolean): Integer; overload;
    { Sets value of "PropName" as an integer on this configuration }
    procedure SetInteger(const PropName: string; const Value: Integer);
    { Evaluates the value of "PropName" up to, but not including, this build
      configuration as a string }
    function InheritedValue(const PropName: string): string;

    { The following methods operate on properties that are lists of items, i.e
      a list of paths }
    { Perform an evaluation the value of property "PropName" as a string list,
      optionally recursing through parent configurations. If IncludeInheritedValues=false,
      the value is determined at this configuration only }
    procedure GetValues(const PropName: string; Values: TStrings;
      IncludeInheritedValues: Boolean = True);
    { For properties that are a list type, return whether or not the property
      "PropName" contains "Value" as one of its elements }
    function ContainsValue(const PropName, Value: string): Boolean;
    { Insert one or more values into a list-type property, at index 'Location' }
    procedure InsertValues(const PropName: string; const Values: array of string; Location: Integer = -1);
    { Replaces all values of list-type property "PropName" with "Values" }
    procedure SetValues(const PropName: string; const Values: TStrings);
    { Removes 'Values' from list-type properties if they exist }
    procedure RemoveValues(const PropName: string; const Values: array of string);
    { Evaluates the value of "PropName" up to, but not including, this build
      configuration as a string list. Only valid for list-type properties }
    procedure InheritedValues(const PropName: string; Values: TStrings; IgnoreMerged: Boolean = False);
    { Return whether or not the property inherits values from its parent
      configuration.  Merging only makes sense for list-type properties.
      To illustrate:
      If a parent configuration defines a property as:
        ListItem1;ListItem2
      and a child configuration defines the same property as:
        ListItem3;ListItem4
      then if the property is merged, the fully-merged value in the child
      configuration would be:
        ListItem3;ListItem4;ListItem1;ListItem2
      If the property is not merged, the value in the child configuration
      would be just:
        ListItem3;ListItem4
      In the project file, merged properties contain a reference to their own
      name as one of their values:
        <PropertyName>ListItem3;Listitem4;$(PropertyName)</PropertyName> }
    function GetMerged(const PropName: string): Boolean;
    { Controls whether or not the property "PropName" has it's value
      merged with inherited values from parent configurations. }
    procedure SetMerged(const PropName: string; Value: Boolean);

    property Name: string read GetName write SetName;
    property Key: string read GetKey;
    property Parent: IOTABuildConfiguration read GetParent;
    property ChildCount: Integer read GetChildCount;
    property Children[Index: Integer]: IOTABuildConfiguration read GetChild;
    property PropertyCount: Integer read GetPropertyCount;
    property Properties[Index: Integer]: string read GetPropertyName;
    property Value[const PropName: string]: string read GetValue write SetValue; default;
    property AsBoolean[const PropName: string]: Boolean read GetBoolean write SetBoolean;
    property AsInteger[const PropName: string]: Integer read GetInteger write SetInteger;
    property Merged[const PropName: string]: Boolean read GetMerged write SetMerged;
  end;

  { Provides access to a project's Build Configurations.  You can query the IOTAProjectOptions
    instance for this interface }
  IOTAProjectOptionsConfigurations = interface(IUnknown)
    ['{E158B38A-90BF-425E-A634-0358B794870E}']
    { Return the number of configurations in this project }
    function GetConfigurationCount: Integer;
    { Return the index'd configuration in this project }
    function GetConfiguration(Index: Integer): IOTABuildConfiguration;
    { Return the active configuration for this project }
    function GetActiveConfiguration: IOTABuildConfiguration;
    { Set the active configuration for this project }
    procedure SetActiveConfiguration(const Value: IOTABuildConfiguration);
    { Return the Base configuration in this project }
    function GetBaseConfiguration: IOTABuildConfiguration;
    { Add a new configuration with the specified name, descending from the specified
      Parent configuration.  Returns the added configuration }
    function AddConfiguration(const Name: string; Parent: IOTABuildConfiguration): IOTABuildConfiguration;
    { Remove the configuration with the specified Name or Key }
    procedure RemoveConfiguration(const Name: string);

    property ConfigurationCount: Integer read GetConfigurationCount;
    property Configurations[Index: Integer]: IOTABuildConfiguration read GetConfiguration;
    property ActiveConfiguration: IOTABuildConfiguration read GetActiveConfiguration write SetActiveConfiguration;
    property BaseConfiguration: IOTABuildConfiguration read GetbaseConfiguration;
  end;

  IOTAProjectBuilder40 = interface(IUnknown)
    ['{F17A7BD5-E07D-11D1-AB0B-00C04FB16FB3}']
    { True if the project is out of date and needs to be built }
    function GetShouldBuild: Boolean;
    { True if successfully built.  If Wait then the compile progress dialog
     waits for the user press OK.  If False, it does not wait if successful. }
    function BuildProject(CompileMode: TOTACompileMode; Wait: Boolean): Boolean;

    property ShouldBuild: Boolean read GetShouldBuild;
  end;

  IOTAProjectBuilder = interface(IOTAProjectBuilder40)
    ['{08A5B1F5-FCDA-11D2-AC82-00C04FB173DC}']
    { True if successfully built.  If Wait then the compile progress dialog
     waits for the user press OK.  If False, it does not wait if successful. }
    function BuildProject(CompileMode: TOTACompileMode; Wait, ClearMessages: Boolean): Boolean; overload;
  end;

  IOTAProject40 = interface(IOTAModule)
    ['{F17A7BCA-E07D-11D1-AB0B-00C04FB16FB3}']
    { Return the number of owned modules }
    function GetModuleCount: Integer;
    { Return the Indexed owned Module Info }
    function GetModule(Index: Integer): IOTAModuleInfo;
    { Return the Project options }
    function GetProjectOptions: IOTAProjectOptions;
    { Return the Project Builder }
    function GetProjectBuilder: IOTAProjectBuilder;

    property ProjectOptions: IOTAProjectOptions read GetProjectOptions;
    property ProjectBuilder: IOTAProjectBuilder read GetProjectBuilder;
  end;

  IOTAProject70 = interface(IOTAProject40)
    ['{06C88136-F367-4D47-B8B4-CCACB3D7439A}']
    { Call this function to add an arbitrary file to the project.  NOTE: some
      files have special meaning to different projects.  For example: adding
      VCL60.DCP will cause a new entry in a package project's "requires" list
      while it will be a raw file to any other project type.  Set IsUnitOrForm
      to true for files that are considered items that the project would
      process directly or indirectly (ie. .pas, .cpp, .rc, etc..) or can be
      opened in the code editor. For all others, including binary files
      (.res, .bpi, .dcp, etc..) set this to False. }
    procedure AddFile(const AFileName: string; IsUnitOrForm: Boolean);
    { Call this function to remove an arbitrary file from the project.  This
      must be a fully qualified filename.  See GetModule() above for info on
      obtaining this information from a Form name or unit name }
    procedure RemoveFile(const AFileName: string);
  end;

  IOTAProject90 = interface(IOTAProject70)
    ['{BBBE4CC6-36DE-4986-BD9E-9DF0F06FC8F1}']
    { This is the same as AddFile but allows you to specify a parent.  A Parent
      is a project Ident, most commonly a filename.  If your file has a parent
      then it will appear under the file in the project manager. If both the
      parent and child has the same file name with different extentions then a
      rename or 'save as' of the parent will also save the child with the same
      base name. }
    procedure AddFileWithParent(const AFileName: string; IsUnitOrForm: Boolean;
      const Parent: string);
    { Each project is assigned a unique GUID.  This method returns this GUID. }
    function GetProjectGUID: TGUID;
    { Each project has a specific personality }
    function GetPersonality: string;
    { Find the ModuleInfo for a given filename, if it exists in the project.
      Otherwise, it returns null }
    function FindModuleInfo(const FileName: string): IOTAModuleInfo;

    property ProjectGUID: TGUID read GetProjectGUID;
    property Personality: string read GetPersonality;
  end;

  IOTAProject100 = interface(IOTAProject90)
    ['{D0090018-D879-41FC-8F83-AA4F40098ACF}']
    { Renames file using the same logic as an inplace rename in
      the project manager. }
    function Rename(const OldFileName, NewFileName: string): Boolean;
  end;

  IOTAProject120 = interface(IOTAProject100)
    ['{3D7E07CB-392D-4EFB-841D-A6C6E338CF13}']
    function GetProjectType: string;

    property ProjectType: string read GetProjectType;
  end;

  IOTAProject = interface(IOTAProject120)
    ['{6B1A57F9-34A3-4824-96F0-750A63328C4E}']
    { Returns a list of fully qualified file names.  This will contain files
      that do not show up in the project manager, for example, the project's 
      .res file }
    procedure GetCompleteFileList(FileList: TStrings);
    { Returns the files associated with FileName.  FileName should be a fully
      qualified name.  The results in FileList will be fully qualified names.
      For example c:\foo.pas could return c:\foo.dfm, or c:\foo.cpp could return
      c:\foo.dfm and c:\foo.h }
    procedure GetAssociatedFiles(const FileName: string; FileList: TStrings);
    { Returns the transaction for a given file.  FileName should be a fully
      qualified name. If, for the example, c:\foo.pas gets renamed to c:\goo.pas,
      and c:\goo.pas subsequently gets renamed to c:\doo.pas, calling this 
      function with FileName='c:\goo.pas' will set InitialName='c:\foo.pas' and
      CurrentName='c:\doo.pas'. If CurrentName is an empty string, this indicates
      that the file was deleted.  The function returns True if the file is found
      in the transaction list and False if it is not found }
    function GetFileTransaction(const FileName: string; var InitialName,
      CurrentName: string): Boolean;
  end;

  IOTAProjectNotifier = interface(IOTAModuleNotifier)
    ['{75A09281-AD20-427B-A506-4712D0A64164}']
    { This notifier will be called when a file/module is added to the project }
    procedure ModuleAdded(const AFileName: string);
    { This notifier will be called when a file/module is removed from the project }
    procedure ModuleRemoved(const AFileName: string);
    { This notifier will be called when a file/module is renamed in the project }
    procedure ModuleRenamed(const AOldFileName, ANewFileName: string);
  end;

  IOTAProjectCurrentFolder = interface
    ['{D86ED6B5-974A-4657-B94A-D34F0597FEDD}']
    function GetCurrentFolderPath: string;
    property CurrentFolderPath: string read GetCurrentFolderPath;
  end;

  IOTAProjectGroup = interface(IOTAModule)
    ['{F17A7BCB-E07D-11D1-AB0B-00C04FB16FB3}']
    { Invoke the Add New Project Dialog }
    procedure AddNewProject;
    { Invoke the Open New Project Dialog }
    procedure AddExistingProject;
    { Return the currently active project }
    function GetActiveProject: IOTAProject;
    { Number of Projects in this project group }
    function GetProjectCount: Integer;
    { Return the Project interface }
    function GetProject(Index: Integer): IOTAProject;
    { Remove the given project from the project group }
    procedure RemoveProject(const AProject: IOTAProject);
    { Set the active project }
    procedure SetActiveProject(const AProject: IOTAProject);

    property ActiveProject: IOTAProject read GetActiveProject write SetActiveProject;
    property ProjectCount: Integer read GetProjectCount;
    property Projects[Index: Integer]: IOTAProject read GetProject;
  end;

  IOTAProjectDependenciesList = interface(IInterface)
    ['{4D87F08B-A6A6-4C0F-AC33-D6CE792EC522}']
    { Add a project to the list }
    procedure AddProject(const AProject: IOTAProject);
    { Returns the number of projects in the list }
    function GetProjectCount: Integer;
    { Returns the index'd project in the list }
    function GetProject(Index: Integer): IOTAProject;
    { Remove a project from the list }
    procedure RemoveProject(const AProject: IOTAProject);

    property ProjectCount: Integer read GetProjectCount;
    property Projects[Index: Integer]: IOTAProject read GetProject;
  end;

  IOTAProjectGroupProjectDependencies = interface(IInterface)
    ['{9B6203FF-7019-49D5-B3DF-6FBDFC61BACE}']
    { Returns an empty IOTAProjectDependenciesList }
    function GetEmptyProjectDependenciesList: IOTAProjectDependenciesList;
    { Return current dependencies AProject depends upon }
    function GetProjectDependencies(const AProject: IOTAProject): IOTAProjectDependenciesList;
    { Return the projects that AProject can validly depend on.  This function will not return
      any circular references. }
    function GetValidProjectDependencies(const AProject: IOTAProject): IOTAProjectDependenciesList;
    { Set the dependencies on AProject }
    procedure SetProjectDependencies(const AProject: IOTAProject; const ADependencies: IOTAProjectDependenciesList);
  end;

  IOTAActionServices = interface(IUnknown)
    ['{F17A7BC9-E07D-11D1-AB0B-00C04FB16FB3}']
    { Action interfaces }
    function CloseFile(const FileName: string): Boolean;
    function OpenFile(const FileName: string): Boolean;
    function OpenProject(const ProjName: string; NewProjGroup: Boolean): Boolean;
    function ReloadFile(const FileName: string): Boolean;
    function SaveFile(const FileName: string): Boolean;
  end;

  IOTAFileFilter = interface(IInterface)
    ['{14DBB1D5-9802-458E-A8C3-606237708939}']
    function GetStream(const AFileName: string; const AStream: IStream): IStream;
    function HandlesStream(const AFileName: string; const AStream: IStream): Boolean;
    function GetIDString: string;
    function GetDisplayName: string;
    property DisplayName: string read GetDisplayName;
    property IDString: string read GetIDString;
  end;

  IOTAFileFilterByName = interface(IOTAFileFilter)
    ['{0624E15E-A4E1-45D6-8D24-FDF67133E483}']
  end;

  IOTAFileFilterWithCheckEncode = interface(IOTAFileFilter)
    ['{17E89149-6F65-416f-88EA-B949449BA9A0}']
    function GetInvalidCharacterException: Boolean;
    procedure SetInvalidCharacterException(const Value: Boolean);
    function GetIgnoreException: Boolean;
    procedure SetIgnoreException(const Value: Boolean);
    property InvalidCharacterException: Boolean read GetInvalidCharacterException write SetInvalidCharacterException;
    property IgnoreException: Boolean read GetIgnoreException write SetIgnoreException;
  end;

  IOTAFileFilterServices = interface(IInterface)
    ['{84302AE6-646C-4547-85F1-4FB0839038E2}']
    function GetDefaultFilter: IOTAFileFilter;
    procedure SetDefaultFilter(const Value: IOTAFileFilter);
    function GetFilterHandler(const FileName: string; const AStream: IStream): IOTAFileFilter;
    function GetFileFilterCount: Integer;
    function GetFileFilter(Index: Integer): IOTAFileFilter;
    function AddFileFilter(const AFileFilter: IOTAFileFilter): Integer;
    function GetMessageGroupName: string;
    procedure RemoveFileFilter(Index: Integer);
    property DefaultFilter: IOTAFileFilter read GetDefaultFilter write SetDefaultFilter;
    property FileFilter[Index: Integer]: IOTAFileFilter read GetFileFilter;
    property FileFilterCount: Integer read GetFileFilterCount;
    property MessageGroupName: string read GetMessageGroupName;
  end;

  { IOTAStreamModifyTime - this interface should be implemented on an IStream if
    the IStream reflects a file-system file.  This allows the IDE to get and set
    the file age.  This is in DOS date-time format. }

  IOTAStreamModifyTime = interface(IInterface)
    ['{49F2F63F-60CB-4FD4-B12F-8167FC79B293}']
    function GetModifyTime: Longint; stdcall;
    procedure SetModifyTime(Time: Longint); stdcall;
  end;

  IOTAFileSystem = interface(IInterface)
    ['{A9D1389D-F4B0-11D1-AB22-00C04FB16FB3}']
    function GetFileStream(const FileName: string; Mode: Integer): IStream;
    function FileAge(const FileName: string): Longint;
    function RenameFile(const OldName, NewName: string): Boolean;
    function IsReadonly(const FileName: string): Boolean;
    function IsFileBased: Boolean;
    function DeleteFile(const FileName: string): Boolean;
    function FileExists(const FileName: string): Boolean;
    function GetTempFileName(const FileName: string): string;
    function GetBackupFileName(const FileName: string): string;
    function GetIDString: string;
  end;

  IOTAFileSystem80 = interface(IOTAFileSystem)
    ['{A9C2BD51-0372-47B6-8831-AA774CD5AC37}']
    function GetFilter: IOTAFileFilter;
  end;

  IOTAFile = interface(IUnknown)
    ['{6E2AD9B0-F7F0-11D1-AB26-00C04FB16FB3}']
    { Return the actual source code }
    function GetSource: string;
    { Return the age of the file. -1 if new }
    function GetAge: TDateTime;

    property Source: string read GetSource;
    property Age: TDateTime read GetAge;
  end;

  IOTACreator = interface(IUnknown)
    ['{6EDB9B9E-F57A-11D1-AB23-00C04FB16FB3}']
    { Return a string representing the default creator type in which to augment.
      See the definitions of sApplication, sConsole, sLibrary and
      sPackage, etc.. above.  Return an empty string indicating that this
      creator will provide *all* information }
    function GetCreatorType: string;
    { Return False if this is a new module }
    function GetExisting: Boolean;
    { Return the File system IDString that this module uses for reading/writing }
    function GetFileSystem: string;
    { Return the Owning module, if one exists (for a project module, this would
      be a project; for a project this is a project group) }
    function GetOwner: IOTAModule;
    { Return true, if this item is to be marked as un-named.  This will force the
      save as dialog to appear the first time the user saves. }
    function GetUnnamed: Boolean;

    property CreatorType: string read GetCreatorType;
    property Existing: Boolean read GetExisting;
    property FileSystem: string read GetFileSystem;
    property Owner: IOTAModule read GetOwner;
    property Unnamed: Boolean read GetUnnamed;
  end;

  IOTAModuleCreator = interface(IOTACreator)
    ['{6EDB9B9A-F57A-11D1-AB23-00C04FB16FB3}']
    { Return the Ancestor form name }
    function GetAncestorName: string;
    { Return the implementation filename, or blank to have the IDE create a new
      unique one. (C++ .cpp file or Delphi unit) NOTE: If a value is returned then it *must* be a
      fully qualified filename.  This also applies to GetIntfFileName and
      GetAdditionalFileName on the IOTAAdditionalFilesModuleCreator interface. }
    function GetImplFileName: string;
    { Return the interface filename, or blank to have the IDE create a new
      unique one.  (C++ header) }
    function GetIntfFileName: string;
    { Return the form name }
    function GetFormName: string;
    { Return True to Make this module the main form of the given Owner/Project }
    function GetMainForm: Boolean;
    { Return True to show the form }
    function GetShowForm: Boolean;
    { Return True to show the source }
    function GetShowSource: Boolean;
    { Create and return the Form resource for this new module if applicable }
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
    { Create and return the Implementation source for this module. (C++ .cpp
      file or Delphi unit) }
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    { Create and return the Interface (C++ header) source for this module }
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    { Called when the new form/datamodule/custom module is created }
    procedure FormCreated(const FormEditor: IOTAFormEditor);

    property AncestorName: string read GetAncestorName;
    property FormName: string read GetFormName;
    property ImplFileName: string read GetImplFileName;
    property IntfFileName: string read GetIntfFileName;
    property MainForm: Boolean read GetMainForm;
    property ShowForm: Boolean read GetShowForm;
    property ShowSource: Boolean read GetShowSource;
  end;

  IOTAAdditionalFilesModuleCreator = interface(IOTAModuleCreator)
    ['{BACD1450-1AC5-11D4-A455-00C04F6BB853}']
    function GetAdditionalFilesCount: Integer;
    { Create and return the source for this additional file }
    function NewAdditionalFileSource(I: Integer; const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    { Return the additional filename, or blank to have the IDE create a new
      unique one. }
    function GetAdditionalFileName(I: Integer): string;
    { Get the file extent used to create a new file name }
    function GetAdditionalFileExt(I: Integer): string;
  end;

  { In order to work properly in the current IDE, each IOTAProjectCreator should
    also implement IOTAProjectCreator80.  Failing to do so will cause your
    creator to either 1) take on the active personality in the IDE or 2) fail if
    there is no active personality at the time your creator is called. }
    
  IOTAProjectCreator = interface(IOTACreator)
    ['{6EDB9B9D-F57A-11D1-AB23-00C04FB16FB3}']
    { Return the project filename. NOTE: This *must* be a fully qualified file name. }
    function GetFileName: string;
    { Deprecated!! Return the option file name (C++ .bpr, .bpk, etc...) }
    function GetOptionFileName: string; deprecated;
    { Return True to show the source }
    function GetShowSource: Boolean;
    { Deprecated!! Called to create a new default module for this project.
      Please implement and use the method on IOTAProjectCreator50. }
    procedure NewDefaultModule; deprecated;
    { Deprecated!! Create and return the project option source. (C++) }
    function NewOptionSource(const ProjectName: string): IOTAFile; deprecated;
    { Called to indicate when to create/modify the project resource file }
    procedure NewProjectResource(const Project: IOTAProject);
    { Create and return the Project source file }
    function NewProjectSource(const ProjectName: string): IOTAFile;

    property FileName: string read GetFileName;
    property OptionFileName: string read GetOptionFileName;
    property ShowSource: Boolean read GetShowSource;
  end;

  IOTAProjectCreator50 = interface(IOTAProjectCreator)
    ['{64312F82-62F3-48E9-BAF6-B03DF450312A}']
    { Called to create a new default module(s) for the given project.  This
      interface method is the preferred mechanism. }
    procedure NewDefaultProjectModule(const Project: IOTAProject);
  end;

  IOTAProjectCreator80 = interface(IOTAProjectCreator50)
    ['{9A1D6AF5-84FA-481C-A446-746D9A50F53E}']
    { Implement this interface and return the correct personality of the project
      to create.  The CreatorType function should return any sub-types that this
      personality can create.  For instance, in the Delphi.Personality, returning
      'Package' from CreatorType will create a proper package project. }
    function GetProjectPersonality: string;

    property ProjectPersonality: string read GetProjectPersonality;
  end;

  IOTAProjectGroupCreator = interface(IOTACreator)
    ['{6EDB9B9F-F57A-11D1-AB23-00C04FB16FB3}']
    { Return the project group file name }
    function GetFileName: string;
    { Return True to show the source }
    function GetShowSource: Boolean;
    { Deprecated/never called.  Create and return the project group source }
    function NewProjectGroupSource(const ProjectGroupName: string): IOTAFile; deprecated;

    property FileName: string read GetFileName;
    property ShowSource: Boolean read GetShowSource;
  end;

  IOTAModuleServices70 = interface(IUnknown)
    ['{F17A7BCD-E07D-11D1-AB0B-00C04FB16FB3}']
    { Registers a new file system }
    function AddFileSystem(FileSystem: IOTAFileSystem): Integer;
    { Close all open modules including the Project Group }
    function CloseAll: Boolean;
    { Given the Creator, create a new module of the implied type }
    function CreateModule(const Creator: IOTACreator): IOTAModule;
    { Return the currently active module }
    function CurrentModule: IOTAModule;
    { Lookup the given file system }
    function FindFileSystem(const Name: string): IOTAFileSystem;
    { Lookup the given module by form name }
    function FindFormModule(const FormName: string): IOTAModule;
    { Lookup the given module by file name }
    function FindModule(const FileName: string): IOTAModule;
    { Return the number of currently open modules }
    function GetModuleCount: Integer;
    { Return an interface on the module at index }
    function GetModule(Index: Integer): IOTAModule;
    { Given the Prefix, create a new unique Module name and class name }
    procedure GetNewModuleAndClassName(const Prefix: string; var UnitIdent,
      ClassName, FileName: string);
    { Open the File|New dialog }
    function NewModule: Boolean;
    { Removes the index'd file system from the installed file system list }
    procedure RemoveFileSystem(Index: Integer);
    { Save all modules. Same as File|Save All }
    function SaveAll: Boolean;

    property ModuleCount: Integer read GetModuleCount;
    property Modules[Index: Integer]: IOTAModule read GetModule;
  end;

  IOTAModuleServices = interface(IOTAModuleServices70)
    ['{55A5E848-27FB-4880-8E7C-7F05A9802482}']
    { Returns the currently open ProjectGroup }
    function GetMainProjectGroup: IOTAProjectGroup;
    { Opens and returns IOTAModule representing the given file.  If the file is
      already open, then that module is returned. If you want to show the module
      you must call IOTAModule.Show, call IOTAModule.ShowFilename, or iterate
      through the module's editors and show one specifically.  If you never show
      the module, it will remain open and participate in any File|Close all, or
      File|Save all operations, which means the IDE may ask you to save the module
      if it is modified. }
    function OpenModule(const FileName: string): IOTAModule;
    { Returns the currently active project.  This will return an "unbound"
      package project if one is open without a project group. }
    function GetActiveProject: IOTAProject;

    property MainProjectGroup: IOTAProjectGroup read GetMainProjectGroup;
  end;

  IOTAProcess = interface;
  IOTAThread = interface;

  TOTATriggerResult = (trStop, trContinue, trDefault);
  TOTAAccessType = (atRead, atWrite, atExecute);
  TOTARunMode = (ormRun,               //run the process
                 ormRunToEntry,        //run to the first source statement executed -- used for a process being started
                 ormRunToMain,         //run to the main entry-point (main, WinMain, etc.) -- used for a process being started
                 ormRunToCursor,       //run to the cursor location in the editor
                 ormStmtStepInto,      //step into source statement
                 ormStmtStepOver,      //step over source statement
                 ormInstStepInto,      //step into machine instruction
                 ormInstStepOver,      //step over machine instruction
                 ormStmtStepToSource,  //step to next source statement
                 ormRunToTerminate,    //run to termination -- identical to ormRun
                 ormRunUntilReturn,    //run until the current function returns
                 ormUnused);           //unused

  IOTABreakpointNotifier = interface(IOTANotifier)
    ['{34B2E2D5-E36F-11D1-AB0E-00C04FB16FB3}']
    { Called when IDE attempts to edit this breakpoint. Return False to allow
      the default edit dialogs to function. AllowKeyChanges is True if all
      Breakpoint parameters are allowed to be changed, False if only certain
      items should be allowed to change. For example; when the user selects
      "Breakpoint properties..." from the right-click menu in the editor, the
      filename and line number fields are read-only.  AllowKeyChanges will be
      False in this case. }
    function Edit(AllowKeyChanges: Boolean): Boolean;
    { Called when this breakpoint is triggered. Return trStop to stop,
      trContinue to continue or trDefault to do the default processing. }
    function Trigger: TOTATriggerResult;
    { Called when the breakpoint is verified }
    procedure Verified(Enabled, Valid: Boolean);
  end;

  IOTABreakpoint40 = interface(IUnknown)
    ['{34B2E2D4-E36F-11D1-AB0E-00C04FB16FB3}']
    { Add a callback notifier.  Returns the index of this notifier in the
      breakpoint's list of notifiers.  Retain this value to be used when
      calling RemoveNotifier }
    function AddNotifier(const Notifier: IOTABreakpointNotifier): Integer;
    { Call this to destroy the object.  This is deprecated.  Use
      IOTADebuggerServices.RemoveBreakpoint to remove a breakpoint. }
    procedure Destruct; deprecated 'Use IOTADebuggerServices.RemoveBreakpoint';
    { Processes default Trigger actions. Returns True to indicate stop.
      This should only be called within the IOTABreakpointNotifier.Trigger
      callback. }
    function DefaultTrigger: Boolean;
    { Decrement the CurPassCount if > 0.  Return True if it goes to 0 or is 0 }
    function DecPassCount: Boolean;
    { Invokes the normal edit processing.  Calling Edit on Address breakpoints
      with an AllowKeyChanges parameter of True can cause the Breakpoint object
      to be deleted and recreated.  To properly handle this, you should install
      a breakpoint notifier and listen for the Deleted notification }
    procedure Edit(AllowKeyChanges: Boolean);
    { Evaluates the expression associated with this breakpoint }
    function EvaluateExpression: Boolean;
    { Returns the Enabled status of this BP. }
    function GetEnabled: Boolean;
    { Retrieves the Expression associated }
    function GetExpression: string;
    { Return the Source file name in-which this Breakpoint is set }
    function GetFileName: string;
    { Return the line number on which this breakpoint is set }
    function GetLineNumber: Integer;
    { Retrieves the current Pass Count if PassCount is set }
    function GetCurPassCount: Integer;
    { Retrieves the associated Pass count }
    function GetPassCount: Integer;
    { Removes the index'th notifier from the breakpoint's list }
    procedure RemoveNotifier(Index: Integer);
    { Resets the CurPassCount back to PassCount }
    procedure ResetPassCount;
    { Sets the filename }
    procedure SetFileName(const Value: string);
    { Sets the Line Number }
    procedure SetLineNumber(Value: Integer);
    { Sets the Enabled status of this breakpoint }
    procedure SetEnabled(Value: Boolean);
    { Sets the Expression associated with this breakpoint }
    procedure SetExpression(const Value: string);
    { Sets the Pass count associated with this breakpoint. Calling this will
      clear the CurPassCount. }
    procedure SetPassCount(Value: Integer);
    { Returns whether this Breakpoint is valid in the currently selected
      process }
    function ValidInCurrentProcess: Boolean;
    { Returns whether this breakpoint is valid in the given process }
    function ValidInProcess(const Process: IOTAProcess): Boolean;

    property CurPassCount: Integer read GetCurPassCount;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Expression: string read GetExpression write SetExpression;
    property FileName: string read GetFileName write SetFileName;
    property LineNumber: Integer read GetLineNumber write SetLineNumber;
    property PassCount: Integer read GetPassCount write SetPassCount;
  end;

  IOTABreakpoint50 = interface(IOTABreakpoint40)
    ['{569EFCFB-C69B-11D2-AC67-00C04FB173DC}']
    { Retrieves the Group Name }
    function GetGroupName: string;
    { Retrieves the Actions }
    function GetDoBreak: Boolean;
    { Retrieves the Message to Log }
    function GetLogMessage: string;
    { Retrieves the Expression to Evaluate }
    function GetEvalExpression: string;
    { Retrieves the LogResult flag }
    function GetLogResult: Boolean;
    { Retrieves the Group to Enable }
    function GetEnableGroup: string;
    { Retrieves the Group to Disable }
    function GetDisableGroup: string;
    { Sets the Group Name }
    procedure SetGroupName(const Value: string);
    { Sets the Actions }
    procedure SetDoBreak(const Value: Boolean);
    { Sets the Message to Log }
    procedure SetLogMessage(const Value: string);
    { Sets the Expression to Evaluate }
    procedure SetEvalExpression(const Value: string);
    { Sets the LogResult flag }
    procedure SetLogResult(const Value: Boolean);
    { Sets the Group to Enable }
    procedure SetEnableGroup(const Value: string);
    { Sets the Group to Disable }
    procedure SetDisableGroup(const Value: string);
    property GroupName: string read GetGroupName write SetGroupName;
    property DoBreak: Boolean read GetDoBreak write SetDoBreak;
    property LogMessage: string read GetLogMessage write SetLogMessage;
    property EvalExpression: string read GetEvalExpression write SetEvalExpression;
    property LogResult: Boolean read GetLogResult write SetLogResult;
    property EnableGroup: string read GetEnableGroup write SetEnableGroup;
    property DisableGroup: string read GetDisableGroup write SetDisableGroup;
  end;

  IOTABreakpoint80 = interface(IOTABreakpoint50)
    ['{446F637B-3EBD-4E33-B011-714CE9647BB9}']
    { Retrieves the "Handle Subsequent Exceptions" flag }
    function GetDoHandleExceptions: Boolean;
    { Retrieves the "Ignore Subsequent Exceptions" flag }
    function GetDoIgnoreExceptions: Boolean;
    { Sets the "Handle Subsequent Exceptions" flag }
    procedure SetDoHandleExceptions(const Value: Boolean);
    { Sets the "Ignore Subsequent Exceptions" flag }
    procedure SetDoIgnoreExceptions(const Value: Boolean);
    property DoHandleExceptions: Boolean read GetDoHandleExceptions write SetDoHandleExceptions;
    property DoIgnoreExceptions: Boolean read GetDoIgnoreExceptions write SetDoIgnoreExceptions;
  end;

  IOTABreakpoint120 = interface(IOTABreakpoint80)
    ['{614D8D87-F3E2-46B7-8033-4E8B37E697BB}']
    { Retrieves the number of Stack Frames to Log }
    function GetStackFramesToLog: Integer;
    { Sets the number of Stack Frames to Log}
    procedure SetStackFramesToLog(const Value: Integer);
    property StackFramesToLog: Integer read GetStackFramesToLog write SetStackFramesToLog;
  end;

  IOTABreakpoint = interface(IOTABreakpoint120)
    ['{8950E1C9-C32F-4132-87DD-62786A3F9904}']
    { Retrieves the thread on which this breakpoint should trigger.  It will be
      a string representing either the OS Thread ID or the Thread Name }
    function GetThreadCondition: string;
    { Sets the thread on which this breakpoint should trigger }
    procedure SetThreadCondition(const Value: string);
    property ThreadCondition: string read GetThreadCondition write SetThreadCondition;
  end;

  IOTASourceBreakpoint = interface(IOTABreakpoint)
    ['{09063877-E43A-11D1-AB0F-00C04FB16FB3}']
  end;

  IOTAAddressBreakpoint = interface(IOTABreakpoint)
    ['{09063878-E43A-11D1-AB0F-00C04FB16FB3}']
    { Returns the start address of this breakpoint in the current process,
      0 if not valid in the current process }
    function Address: LongWord;
    { Returns the start address of this breakpoint in the given process,
      0 if not valid in the given process }
    function AddressInProcess(const Process: IOTAProcess): LongWord;
    { Returns the Access type Write or Execute }
    function GetAccessType: TOTAAccessType;
    { Returns the Data Expression if this is a DW Breakpoint, empty string if
      not }
    function GetDataExpr: string;
    { Returns the Size of the line if this address breakpoint was mapped to
      a source line }
    function GetLineSize: Integer;
    { Returns the Offset in the line if this address breakpoint was mapped to
      a source line }
    function GetLineOffset: Integer;
    { Returns the Process Module this BP will stop on the entry of if a Module
      LoadBP.  If this is a Data Watch BP, this is the Exe module name that
      this breakpoint is valid for. }
    function GetModuleName: string;

    property AccessType: TOTAAccessType read GetAccessType;
    property DataExpr: string read GetDataExpr;
    property ModuleName: string read GetModuleName;
    property LineSize: Integer read GetLineSize;
    property LineOffset: Integer read GetLineOffset;
  end;

  TOTANotifyReason = (nrOther, nrRunning, nrStopped, nrException, nrFault);

  IOTAThreadNotifier = interface(IOTANotifier)
    ['{34B2E2D7-E36F-11D1-AB0E-00C04FB16FB3}']
    { This is called when the process state changes for this thread }
    procedure ThreadNotify(Reason: TOTANotifyReason);
    { This is called when an evaluate that returned erDeferred completes.
      ReturnCode <> 0 if error }
    procedure EvaluteComplete(const ExprStr, ResultStr: string; CanModify: Boolean;
      ResultAddress, ResultSize: LongWord; ReturnCode: Integer);
    { This is called when a modify that returned erDeferred completes.
      ReturnCode <> 0 if error }
    procedure ModifyComplete(const ExprStr, ResultStr: string; ReturnCode: Integer);
  end;

  { erOK       - indicates evaluate operation was successful
    erError    - indicates evaluate operation was unsuccessful
    erDeferred - indicates evaluate operation is deferred
    erBusy     - indicates evaluate operation was not attempted due to the
                 evaluator already processing another evaluate operation }
  TOTAEvaluateResult = (erOK, erError, erDeferred, erBusy);

  TOTAThreadState = (tsStopped, tsRunnable, tsBlocked, tsNone, tsOther);

  IOTAThread50 = interface(IUnknown)
    ['{34B2E2D3-E36F-11D1-AB0E-00C04FB16FB3}']
    { Add an IOTAThreadNotifier }
    function AddNotifier(const Notifier: IOTAThreadNotifier): Integer;
    { Evaluate the given expression.
      CanModify -        will be set to true if the expression can be modified.
      AllowSideEffects - indicates whether the evaluator is allowed to perform
                         any function calls in the running process in order to
                         complete the evaluation. This includes property access
                         methods.
      FormatSpecifiers - String indicating format overrides. See IDE help for
                         definition
      ResultAddr       - Will be set if the expression evaluates to an address
                         within the process
      ResultSize       - Size of the expression. (ie. SizeOf(<sometype>))

      Function result will be erDeferred if the evaluator had to make a
      function call in the running process in order to finish the evaluation.
      The results are undefined in this case.  See the EvaluateComplete method
      on the IOTAThreadNotifier.  If the result is erError, the ResultStr may
      contain an error message. }
    function Evaluate(const ExprStr: string; ResultStr: PChar; ResultStrSize: LongWord;
      out CanModify: Boolean; AllowSideEffects: Boolean; FormatSpecifiers: PAnsiChar;
      out ResultAddr: LongWord; out ResultSize, ResultVal: LongWord): TOTAEvaluateResult;
    { Modify the last evaluated expression.
      ResultStr and ResultVal will be set the to the evaluated ValueStr values.
      Like Evaluate, the result could be erDeferred. See the ModifyComplete
      method on the IOTAThreadNotifier.  If the result is erError then ResultStr
      may contain an error message. }
    function Modify(const ValueStr: string; ResultStr: PChar; ResultSize: LongWord;
      out ResultVal: Integer): TOTAEvaluateResult;
    { Return the number of Items on the call stack }
    function GetCallCount: Integer;
    { Return the evaluator generated string for the given stack index.  GetCallCount
      must be called before GetCallHeader. CallHeaders are one-based (not zero-based)
      so the first item in the list is at index 1 }
    function GetCallHeader(Index: Integer): string;
    { Return the source file name and line number of the given stack index.  If the
      frame at the given index does not correspond to a source location, FileName will
      be an empty string and LineNum will be zero }
    procedure GetCallPos(Index: Integer; out FileName: string; out LineNum: Integer);
    { If the process is stopped, return which file the process is stopped on in
      this thread.  The return string will be blank if no debug info is found at
      this location }
    function GetCurrentFile: string;
    { If the process is stopped, return the line number in the above file at
      which this process is stopped }
    function GetCurrentLine: LongWord;
{$IFDEF MSWINDOWS}
    { Return the Current Thread context }
    function GetContext: TContext;
{$ENDIF}
    { Return the OS Thread Handle }
    function GetHandle: THandle;
    { Return the OS Thread ID }
    function GetOSThreadID: LongWord;
    { Return the current thread State }
    function GetState: TOTAThreadState;
    { Remove the Index'd notifier }
    procedure RemoveNotifier(Index: Integer);

    property CallCount: Integer read GetCallCount;
    property CallHeaders[Index: Integer]: string read GetCallHeader;
    property CurrentFile: string read GetCurrentFile;
    property CurrentLine: LongWord read GetCurrentLine;
{$IFDEF MSWINDOWS}
    property Context: TContext read GetContext;
{$ENDIF}
    property Handle: THandle read GetHandle;
    property State: TOTAThreadState read GetState;
  end;

  IOTAThread60 = interface(IOTAThread50)
    ['{2646D502-95F8-4E6F-A1EC-976E9663C9B6}']
    function GetOTAThreadContext: TOTAThreadContext;
    property OTAThreadContext: TOTAThreadContext read GetOTAThreadContext;
  end;

  IOTAThread70 = interface(IOTAThread60)
    ['{24064FD3-5D3C-D611-88BC-00C04FA06AFC}']
    { Gets the current state of the XMM (SSE) registers.  Returns
      True if the registers are valid.  Returns False is the
      registers are invalid. }
    function GetOTAXMMRegisters(var OTAXMMRegs: TOTAXMMRegs): Boolean;
    { Sets the XMM (SSE) registers to new values. }
    procedure SetOTAXMMRegisters(NewOTAXMMRegs: TOTAXMMRegs);
  end;

  TOTACallStackState = (csAccessible, csInaccessible, csWait);

  TOTAEvalSideEffects = (eseNone, eseAll, esePropertiesOnly);

  IOTAThread90 = interface(IOTAThread70)
    ['{175F985B-4F54-41B2-A0A1-54F3B66ECD07}']
    { Tells the thread that you are about to access the call stack.
      Returns the current state of the call stack for this thread. Check the state
      before using the GetCallCount, GetCallHeader, or GetCallPos methods.
      csAccessible -- stack can be queried
      csInaccessible -- stack can not be queried
      csWait -- stack is temporarily unavailable -- try again after letting the
                message loop spin for a bit (i.e post yourself a message to try again) }
    function StartCallStackAccess: TOTACallStackState;
    { Tells the thread that you are done accessing the call stack }
    procedure EndCallStackAccess;

    { Evaluate the given expression.
      This overloaded version adds a FileName and LineNumber parameter which tells
      the evaluator to uses that source location as the scope in which to evaluate
      the expression }
    function Evaluate(const ExprStr: string; ResultStr: PChar; ResultStrSize: LongWord;
      out CanModify: Boolean; SideEffects: TOTAEvalSideEffects; FormatSpecifiers: PAnsiChar;
      out ResultAddr: LongWord; out ResultSize, ResultVal: LongWord;
      FileName: string; LineNumber: Integer): TOTAEvaluateResult; overload;
    { Gets the string displayed in the "Thread Id" column of the thread view
      for this thread.  If am empty string is returned, the value returned by
      GetOSThreadID will be displayed }
    function GetDisplayString: string;
    { Gets the string to be displayed in the "Location" column of the thread
      view for this thread. This string is only used if CurrentFile is an
      empty string }
    function GetLocationString: string;
    { Gets the process which owns this thread }
    function GetOwningProcess: IOTAProcess;
    { Gets the string to be displayed in the "State" column of the thread view
      for this thread.  This string is only used if GetState returns tsOther }
    function GetStateString: string;
    { Gets the string to be displayed in the "Status" column of the thread view
      for this thread. }
    function GetStatusString: string;

    property DisplayString: string read GetDisplayString;
    property Location: string read GetLocationString;
    property OSThreadID: LongWord read GetOSThreadID;
    property OwningProcess: IOTAProcess read GetOwningProcess;
    property StateString: string read GetStateString;
    property Status: string read GetStatusString;
  end;

  IOTAThread110 = interface(IOTAThread90)
    ['{3A96CD8F-A5CD-4AFE-8A73-DAE1265095D9}']
    { Return the evaluator generated string for the given stack index.  GetCallCount
      must be called before GetSimpleCallHeader. CallHeaders are one-based (not zero-based)
      so the first item in the list is at index 1.
      This differs from GetCallHeader in that it returns stack frame names that
      are not fully qualified (method names only, no namespace, class, etc. names }
    function GetSimpleCallHeader(Index: Integer): string;

    property SimpleCallHeaders[Index: Integer]: string read GetSimpleCallHeader;
  end;

  TThreadWaitChainInfo = record
    CycleDetected: Boolean;
    IsBlocked: Boolean;
    BlockedString: string;
  end;

  IOTAThread120 = interface(IOTAThread110)
    ['{DF4C57A6-8674-4D7E-B75E-6704BB5F1A54}']
    { Retrieves info regarding the thread's wait chain.  Wait chains are only
      available on Vista (or later).  The return value indicates whether a wait
      chain is available for this thread. }
    function WaitChainInfo(var ThreadWaitChainInfo: TThreadWaitChainInfo): Boolean;
  end;

  IOTAThread = interface(IOTAThread120)
    ['{BC146984-1E20-4695-879A-25E6A82F52F7}']
    { Indicates if this thread supports being frozen }
    function GetCanFreeze: Boolean;
    { Indicates if this thread is currently frozen }
    function GetFrozen: Boolean;
    { Freezes the thread }
    procedure Freeze;
    { Thaws the thread }
    procedure Thaw;
    { Asks the thread's evaluator if a given type is a descendant of another
      type. Returns True if ChildType is a descendant of ParentType.  Returns
      False otherwise }
    function IsDescendantOf(const ChildType, ParentType: string): Boolean;
    { Returns the name of the Thread, if the thread is named for debugging.
      Returns an empty string. if the thread is not named. }
    function GetThreadName: string;
    { Sets the name of the thread for debugging }
    procedure SetThreadName(const Name: string);

    property CanFreeze: Boolean read GetCanFreeze;
    property Frozen: Boolean read GetFrozen;
    property ThreadName: string read GetThreadName write SetThreadName;
  end;

  INTAThread = interface
    ['{381708B8-A0FA-44DC-B173-3328AEFA0432}']
    { This tells the thread to show a non-source location. FrameNumber is one-
      based (top frame is one).  If FrameNumber is 0 or 1, the top frame is used
      Otherwise, the thread should show the location of the frame indicated.  Use
      GetCallCount to get the number of available frames for a thread. The
      BehindWindow, if not nil, should stay on top after the location is shown }
    procedure ShowNonSourceLocation(FrameNumber: Integer; BehindWindow: TCustomForm);
  end;

  IOTAProcessModNotifier = interface(IOTANotifier)
    ['{0906387A-E43A-11D1-AB0F-00C04FB16FB3}']
    { Modified is called as evaluator symbols for this module are loaded }
  end;

  IOTAProcessModule80 = interface(IUnknown)
    ['{09063879-E43A-11D1-AB0F-00C04FB16FB3}']
    { Adds an IOTAProcessModNotifier }
    function AddNotifier(const Notifier: IOTAProcessModNotifier): Integer;
    { Returns the number of Compilation Units that comprise this Process Module }
    function GetCompUnitCount: Integer;
    { Returns the index'd Compilation Unit Name }
    function GetCompUnit(Index: Integer): string;
    { Returns the number of files used to create the index'd compilation unit }
    function GetCompUnitFileCount(Index: Integer): Integer;
    { Returns the index'd filename of the index'd compilation unit }
    function GetCompUnitFileName(CompIndex, FileIndex: Integer): string;
    { Returns the address of the first code instruction for this module }
    function GetEntryPoint: LongWord;
    { Returns the Base Load address of the module }
    function GetBaseAddress: LongWord;
    { Returns the number of associated source files in this process module }
    function GetFileCount: Integer;
    { Return the index'd file }
    function GetFileName(Index: Integer): string;
    { Returns the number of entry points (procedure/functions/exports) in this
      Process Module }
    function GetModuleEntryPointCount: Integer;
    { Returns the index'd module entry point name }
    function GetModuleEntryPoint(Index: Integer): string;
    { Returns the index'd module entry point address }
    function GetModuleEntryPointAddress(Index: Integer): LongWord;
    { FileName of the Exe/Dll/Bpl, etc.. }
    function GetModuleFileName: string;
    { ModuleName of the Exe/Dll/Bpl, etc }
    function GetModuleName: string;
    { Removed the index's IOTAProcessModNotifier }
    procedure RemoveNotifier(Index: Integer);

    property CompUnitCount: Integer read GetCompUnitCount;
    property CompUnit[Index: Integer]: string read GetCompUnit;
    property CompUnitFileCount[Index: Integer]: Integer read GetCompUnitFileCount;
    property CompUnitFileName[CompIndex, FileIndex: Integer]: string read GetCompUnitFileName;
    property FileCount: Integer read GetFileCount;
    property FileNames[Index: Integer]: string read GetFileName;
    property ModuleEntryPointCount: Integer read GetModuleEntryPointCount;
    property ModuleEntryPoint[Index: Integer]: string read GetModuleEntryPoint;
    property ModuleEntryPointAddress[Index: Integer]: LongWord read GetModuleEntryPointAddress;
  end;

  TOTAEntryPointSortType = (epsByName, epsByAddress);
  TOTAEntryPointSortDirection = (epsAscending, epsDescending);

  IOTAProcessModule90 = interface(IOTAProcessModule80)
    ['{9B4A6BC8-CC15-42A9-A41D-816A72CA0AF1}']
    { Indicates whether or not this ProcessModule's symbol table can be reloaded }
    function CanReloadSymbolTable: Boolean;
    { Ask the ProcessModule to reload its symbol table }
    procedure ReloadSymbolTable(const NewPath: string);
    { Gets the fully qualified file name associated with the file index.  This will
      search along the search path and prompt the user for the location if it can't
      be found }
    function SearchFileNameFromIndex(Index: Integer): string;
    { Ask the ProcessModule to sort its entry points.  HowToSort indicates which part of the
      entry point to sort on.  epsByName indicates that the the entry points should be sorted
      alphabetically by entry point name.  epsByAddress indicated that the entry points should
      be sorted numerically by entry point address. Direction idicates whether the sort should
      be done in ascending (epsAscending) or descending (epsDescending) order }
    procedure SortEntryPoints(HowToSort: TOTAEntryPointSortType; Direction: TOTAEntryPointSortDirection);
    { Tells the process module to navigate to a particular entry point }
    procedure ShowEntryPoint(Index: Integer);
  end;

  IOTAProcessModule110 = interface(IOTAProcessModule90)
    ['{EA1D9277-C318-4E5C-8BDC-03529E81DF8E}']
    { Indicates whether or not this module has debug symbol information available }
    function GetHasSymbols: Boolean;
    { Returns the fully qualified name of the symbol table file used for this module }
    function GetSymbolFileName: string;

    property HasSymbols: Boolean read GetHasSymbols;
    property SymbolFileName: string read GetSymbolFileName;
  end;

  IOTAProcessModule = interface(IOTAProcessModule110)
    ['{41171E69-E830-40B4-A8C6-1A115307C8A1}']
    { Indicates whether or not this module contains debug information for
      the specified source file.  This method can be used as more efficient
      alternative to IOTAProcess.GetSourceIsDebuggable, as this method will
      only look in a given process module, while IOTAProcess.GetSourceIsDebuggable
      will search all process modules in a given process. }
    function ContainsSourceFile(const FileName: string): Boolean;
  end;

  IOTAProcessNotifier = interface(IOTANotifier)
    ['{34B2E2D6-E36F-11D1-AB0E-00C04FB16FB3}']
    { Called when a Thread is created }
    procedure ThreadCreated(const Thread: IOTAThread);
    { Called when a Thread is Destroyed }
    procedure ThreadDestroyed(const Thread: IOTAThread);
    { Called when a Process module is added }
    procedure ProcessModuleCreated(const ProcessModule: IOTAProcessModule);
    { Called when a Process module is deleted }
    procedure ProcessModuleDestroyed(const ProcessModule: IOTAProcessModule);
  end;

  IOTAProcessNotifier90 = interface(IOTAProcessNotifier)
    ['{E2725B23-E67C-4CF1-B928-FA0F5B9C2C29}']
    { Called after a process' current thread is changed }
    procedure CurrentThreadChanged(const Thread: IOTAThread);
    { Called when a process' thread list changes.  This notification is purposely
      vague.  Things like thread creation, thread destruction, thread attribute
      changes (for now just thread name) are reported via this notification }
    procedure ThreadListChanged(const Process: IOTAProcess);
  end;

  IOTAProcess60 = interface(IUnknown)
    ['{34B2E2D2-E36F-11D1-AB0E-00C04FB16FB3}']
    { Adds an IOTAProcessNotifier }
    function AddNotifier(const Notifier: IOTAProcessNotifier): Integer;
    { Return the currently active thread }
    function GetCurrentThread: IOTAThread;
    { Return the number of Thread in this process }
    function GetThreadCount: Integer;
    { Return the index'd Thread }
    function GetThread(Index: Integer): IOTAThread;
    { Get the debugger's internal process ID }
    function GetProcessId: LongWord;
    { Stop/Pause the process }
    procedure Pause;
    { Read the process memory at the given address }
    function ReadProcessMemory(Address: LongWord; Count: Integer; var Buffer): Integer;
    { Removed the index's IOTAProcessNotifier }
    procedure RemoveNotifier(Index: Integer);
    { Run the process with the specified run mode }
    procedure Run(RunMode: TOTARunMode);
    { Set a new current thread }
    procedure SetCurrentThread(Value: IOTAThread);
    { Reset/Terminate the process }
    procedure Terminate;
    { Write to the process memory at the given address }
    function WriteProcessMemory(Address: LongWord; Count: Integer; var Buffer): Integer;

    property CurrentThread: IOTAThread read GetCurrentThread write SetCurrentThread;
    property ProcessId: LongWord read GetProcessId;
    property ThreadCount: Integer read GetThreadCount;
    property Threads[Index: Integer]: IOTAThread read GetThread;
  end;

  IOTAProcess70 = interface(IOTAProcess60)
    ['{64FC3321-BEC8-4E88-B17A-3E78EA15F10E}']
    { Get the OS process ID }
    function GetOSProcessId: LongWord;

    property OSProcessId: LongWord read GetOSProcessId;
  end;

  TOTAProcessState = (psNothing, psRunning, psStopping, psStopped,
    psFault, psResFault, psTerminated, psException, psNoProcess);

  TGetSrcLinesFunc = function (LineNum: Integer; ClientArg: Pointer): Integer {$IFDEF LINUX} cdecl; {$ENDIF} {$IFDEF MSWINDOWS} pascal; {$ENDIF}

  IOTAProcess90 = interface(IOTAProcess70)
    ['{BEBD67CA-F6FC-44A7-ACBF-E314DB085827}']
    { Indicates if process properties can be set on this process (via the thread view) }
    function CanSetProperties: Boolean;
    { Called when the user asks to set properties for this process (via the thread view) }
    procedure SetProperties;
    { Get the string displayed in the "Thread Id" column of the thread view for
      this process.  If an empty string is returned, the thread view will display the
      ExeName and the OSProcessId. }
    function GetDisplayString: string;
    { Get the name of the executable being debugged }
    function GetExeName: string;
    { Get the string displayed in the "Location" column of the thread view for
      this process.}
    function GetLocationString: string;
    { Get the string displayed in the "State" column in the thread view for
      this process }
    function GetStateString: string;
    { Get the string displayed in the "Status" column in the thread view for
      this process }
    function GetStatusString: string;
    { Get the current process state }
    function GetProcessState: TOTAProcessState;
    { Set the current process state }
    procedure SetProcessState(const NewState: TOTAProcessState);
    { Indicates whether this process has debug info for the specified source file.
      GetSourceLines should only be called on a file if SourceIsDebuggable returns True.
      See also IOTAProcessModule.ContainsSourceFile, which can be a more efficient
      alternative to this method. }
    function GetSourceIsDebuggable(const FileName: string): Boolean;
    { Iterates through the breakpointable source lines in the specified file,
      starting at the specified line.  The PostFunc is called for each line
      on which a breakpoint can be set.  ClientArg will be passed as a parameter
      to the PostFunc }
    procedure GetSourceLines(const FileName: string; StartLine: Integer; PostFunc: TGetSrcLinesFunc; ClientArg: Pointer);

    { Indicates whether the specified breakpoint is valid in this process }
    function BreakpointIsValid(const Breakpoint: IOTASourceBreakpoint): Boolean;

    { Returns the number of IOTAProcessModules loaded by this process }
    function GetProcessModuleCount: Integer;
    { Returns the index'd ProcessModule loaded by this process }
    function GetProcessModule(ModuleIndex: Integer): IOTAProcessModule;

    property DisplayString: string read GetDisplayString;
    property ExeName: string read GetExeName;
    property Location: string read GetLocationString;
    property ProcessModuleCount: Integer read GetProcessModuleCount;
    property ProcessModules[ModuleIndex: Integer]: IOTAProcessModule read GetProcessModule;
    property ProcessState: TOTAProcessState read GetProcessState write SetProcessState;
    property SourceIsDebuggable[const FileName: string]: Boolean read GetSourceIsDebuggable;
    property State: string read GetStateString;
    property Status: string read GetStatusString;
  end;

  IOTAProcess = interface(IOTAProcess90)
    ['{1C540740-E350-4DD1-B026-C233D1C380D4}']
    { Detaches the debugger from the process }
    procedure Detach;
    { Returns the index of the specified ProcessModule (in the "ProcessModules" property) }
    function IndexOfProcessModule(const ProcessModule: IOTAProcessModule): Integer;
    { Returns the source file name and line number associated with the given address. If
      a source location can not be found, FileName will come back as an empty string }
    procedure SourceLocationFromAddress(const Address: LongWord;
      out FileName: string; out LineNum: Integer);
  end;

  INTAProcess = interface
    ['{D65533D2-52B1-460F-ABF1-48CA415524F5}']
    { Asks the debugger to show the code location that corresponds to the specified address
      within this process.  The BehindWindow, if not nil, should stay on top after the
      location is shown. }
    procedure ShowNonSourceLocation(const Address: LongWord; BehindWindow: TCustomForm);
    { Asks the debugger to show the memory location that corresponds to the specified
      address within this process.  The BehindWindow, if not nil, should stay on top after the
      location is shown }
    procedure ShowMemoryLocation(const Address: LongWord; BehindWindow: TCustomForm);
  end;

  IOTADebuggerNotifier = interface(IOTANotifier)
    ['{34B2E2D8-E36F-11D1-AB0E-00C04FB16FB3}']
    { Called when a process is created }
    procedure ProcessCreated(const Process: IOTAProcess);
    { Called when a process is Destroyed }
    procedure ProcessDestroyed(const Process: IOTAProcess);
    { Called when a Breakpoint is Added }
    procedure BreakpointAdded(const Breakpoint: IOTABreakpoint);
    { Called when a breakpoint is Deleted }
    procedure BreakpointDeleted(const Breakpoint: IOTABreakpoint);
  end;

  IOTADebuggerNotifier90 = interface(IOTADebuggerNotifier)
    ['{68558E84-A7EC-499F-AD08-CB00876AC5BE}']
    { Called after an existing breakpoint is changed }
    procedure BreakpointChanged(const Breakpoint: IOTABreakpoint);
    { Called after the current process is changed }
    procedure CurrentProcessChanged(const Process: IOTAProcess);
    { Called after a process' state is changed }
    procedure ProcessStateChanged(const Process: IOTAProcess);
    { Called before a debugger launches a project's process.  This gets called
      whether or not IntegratedDebugging is enabled.  Result is whether the
      program should actually be launched.  In all normal circumstances, you
      should return True from here }
    function BeforeProgramLaunch(const Project: IOTAProject): Boolean;
    { Called when memory within a process has changed in response to a user
      action. Events that trigger this notifier include:
        1. the user changing a variable value in the evaluator/inspector/etc.
        2. the user changes raw data in the CPU view's dump pane, register pane
           or flags pane }
    procedure ProcessMemoryChanged;
  end;

  IOTADebuggerNotifier100 = interface(IOTADebuggerNotifier90)
    ['{FE684C77-220C-4999-ACFA-C4C0C7FB6A42}']
    { called when the global (global in the sense that they are not process-specific) 
      debugger-specific options have been changed }
    procedure DebuggerOptionsChanged;
  end;

  IOTADebuggerNotifier110 = interface(IOTADebuggerNotifier100)
    ['{20E36B81-E987-4947-AAFA-AC0E2F0E72EF}']
    procedure ProcessMemoryChanged(EIPChanged: Boolean);
  end;

  TEnumerateProcessesCallback = procedure (Pid: LongWord; const ProcessName: string; Param: Pointer);

  IOTADebuggerServices60 = interface(IUnknown)
    ['{0E3B9D7A-E119-11D1-AB0C-00C04FB16FB3}']
    { Adds an IOTADebuggerNotifier }
    function AddNotifier(const Notifier: IOTADebuggerNotifier): Integer;
    { Attaches to an existing process }
    procedure AttachProcess(Pid: Integer; const RemoteHost: string = '');
    { Creates a new process }
    procedure CreateProcess(const ExeName, Args: string; const RemoteHost: string = '');
    { Enumerates all the current running processes on the local machine or remote
      machine }
    procedure EnumerateRunningProcesses(Callback: TEnumerateProcessesCallback;
      Param: Pointer; const HostName: string = '');
    { Returns the count of address breakpoints }
    function GetAddressBkptCount: Integer;
    { Returns the index'd Address Breakpoint }
    function GetAddressBkpt(Index: Integer): IOTAAddressBreakpoint;
    { Returns the currently active process. This is set whenever a debug
      event occurs }
    function GetCurrentProcess: IOTAProcess;
    { Returns the number of active processes }
    function GetProcessCount: Integer;
    { Returns the Index'd process }
    function GetProcess(Index: Integer): IOTAProcess;
    { Returns the count of source breakpoints }
    function GetSourceBkptCount: Integer;
    { Returns the index'd source Breakpoint }
    function GetSourceBkpt(Index: Integer): IOTASourceBreakpoint;
    { Log event string to Event log window if present. }
    procedure LogString(const LogStr: string);
    { Creates a new Address breakpoint, will create a source BP if the
      address given is on a line boundary }
    function NewAddressBreakpoint(Address, Length: LongWord; AccessType: TOTAAccessType;
      const AProcess: IOTAProcess = nil): IOTABreakpoint;
    { Creates a new Module load Breakpoint. This is deprecated.  Use new overloaded
      version that takes only a ModuleName }
    function NewModuleBreakpoint(const ModuleName: string;
      const AProcess: IOTAProcess): IOTABreakpoint; deprecated 'Use overloaded NewModuleBreakpoint(const ModuleName: string)';
    { Creates a new Source Breakpoint }
    function NewSourceBreakpoint(const FileName: string; LineNumber: Integer;
      const AProcess: IOTAProcess): IOTABreakpoint;
    { Removed the index'd IOTADebuggerNotifier }
    procedure RemoveNotifier(Index: Integer);
    { Sets the current process }
    procedure SetCurrentProcess(const Process: IOTAProcess);

    property AddressBkptCount: Integer read GetAddressBkptCount;
    property AddressBkpts[Index: Integer]: IOTAAddressBreakpoint read GetAddressBkpt;
    property CurrentProcess: IOTAProcess read GetCurrentProcess write SetCurrentProcess;
    property ProcessCount: Integer read GetProcessCount;
    property Processes[Index: Integer]: IOTAProcess read GetProcess;
    property SourceBkptCount: Integer read GetSourceBkptCount;
    property SourceBkpts[Index: Integer]: IOTASourceBreakpoint read GetSourceBkpt;
  end;

  // TLogItemType's are used for drawing event log messages in different colors.
  TLogItemType = (litDefault,
    {$IFDEF MSWINDOWS}
    litODS, litWMSent, litWMPosted, litOleClientStart, litOleServerStart,
    litOleClientEnd, 
    {$ENDIF}
    litSourceBreakpoint, litLogBreakEval, litBreakpointMessage, litProcStart,
    litProcExit, litThreadStart, litThreadExit, litModLoad, litModUnload,
    litExceptFirstTry);

  IOTADebuggerServices90 = interface(IOTADebuggerServices60)
    ['{A797823A-6BD7-41A4-B36B-3A831A737B2D}']
    { Log event string to Event log window associated with the TLogItemType.
      TLogItemType's are colored differently in the event log. }
    procedure LogString(const LogStr: string; LogItemType: TLogItemType);
  end;

  IOTADebuggerServices120 = interface(IOTADebuggerServices90)
    ['{29AE42CE-006A-4A96-A0BB-0D63D9E83A5C}']
    { Attaches to an existing process.  Same as IOTADebuggerServices60 version,
      adding two parameters. PauseAfterAttach indicates if the process should
      be paused after it is attached to.  DetachOnReset indicates if the
      debugger should detach from the process rather than terminate it when
      the user performs a Program Reset }
    procedure AttachProcess(Pid: Integer; PauseAfterAttach: Boolean;
      DetachOnReset: Boolean; const RemoteHost: string = '');
  end;

  { This is the base for debugger visualizers.  This interface allows you to
    specify a name, a unique identifier, and a description for your visualizer.
    It also allows to specify which types the visualizer will handle }
  IOTADebuggerVisualizer = interface
    ['{744B7632-9F86-49B6-AED5-7A48DA25E376}']
    { Return the number of types supported by this visualizer }
    function GetSupportedTypeCount: Integer;
    { Return the Index'd Type.  TypeName is the type.  AllDescendants indicates
      whether or not types descending from this type should use this visualizer
      as well. }
    procedure GetSupportedType(Index: Integer; var TypeName: string;
      var AllDescendants: Boolean);
    { Return a unique identifier for this visualizer.  This identifier is used
      as the keyname when storing data for this visualizer in the registry.  It
      should not be translated }
    function GetVisualizerIdentifier: string;
    { Return the name of the visualizer to be shown in the Tools | Options dialog }
    function GetVisualizerName: string;
    { Return a description of the visualizer to be shown in the Tools | Options dialog }
    function GetVisualizerDescription: string;

    property VisualizerIdentifier: string read GetVisualizerIdentifier;
    property VisualizerName: string read GetVisualizerName;
    property VisualizerDescription: string read GetVisualizerDescription;
  end;

  { This is the simplest form of a debug visualizer.  With it, you can replace
    the value returned by the evaluator with a more meaningful value.  The
    replacement value will appear in the normal debugger UI (i.e. Evaluator
    Tooltips, Watch View, Locals View, Evaluate/Modify dialog,
    Debug Inspector View).
    There can be only one active IOTADebuggerVisualizerValueReplacer per type }
  IOTADebuggerVisualizerValueReplacer = interface(IOTADebuggerVisualizer)
    ['{6BBFB765-E76F-449D-B059-A794FA06F917}']
    function GetReplacementValue(const Expression, TypeName, EvalResult: string): string;
  end;

  TOTAVisualizerClosedProcedure = procedure of object;

  TOTAVisualizerUnavailableReason = (ovurProcessRunning, ovurOutOfScope);

  { This interface provides communication between the debugger and an
    external debugger visualizer }
  IOTADebuggerVisualizerExternalViewerUpdater = interface
    ['{4FA77EAB-4BA4-4203-B451-3B3C5B428D39}']
    { Called when the visualizer should be closed (because the thread that
      created it has gone away) }
    procedure CloseVisualizer;
    { Called when the process is running or otherwise in a state where it is
      unable to show data.  The recommended action is to show "Process not
      accessible", "Out of scope", or something similar (like is shown in the
      Watch window) }
    procedure MarkUnavailable(Reason: TOTAVisualizerUnavailableReason);
    { Called when the data for the visualizer needs to be refreshed }
    procedure RefreshVisualizer(const Expression, TypeName, EvalResult: string);
    { Called to set a callback that should be called to let the debugger know
      that the external viewer has been closed.  The CloseProc is a debugger-
      provided procedure that the visualizer UI should call when the user closes
      it.  This tells the debugger (among other things) to stop refreshing this
      visualizer }
    procedure SetClosedCallback(ClosedProc: TOTAVisualizerClosedProcedure);
  end;

  { This visualizer type allows you to create/show separate UI for the specified
    data types.
    There can be many active IOTADebuggerVisualizerExternalViewers per type }
  IOTADebuggerVisualizerExternalViewer = interface(IOTADebuggerVisualizer)
    ['{A0D5CAF5-83A3-446E-B040-B3E40A926A72}']
    { Returns the text to be used in the menu caption for this visualizer }
    function GetMenuText: string;
    { Show is called when the user selects this visualizer type from the list
      of the installed visualizers for a given type.  You should create and
      show the UI of your visualizer and return an interface that is used
      by the debugger to communicate with the visualizer. "SuggestedLeft" and
      "SuggestedTop" are the suggested screen coordinates for the visualizer }
    function Show(const Expression, TypeName, EvalResult: string;
      SuggestedLeft, SuggestedTop: Integer): IOTADebuggerVisualizerExternalViewerUpdater;
  end;

  IOTADebuggerServices = interface(IOTADebuggerServices120)
    ['{DC682429-BB92-4AF7-9E62-26557D68DE75}']
    { Returns the number of Module Load breakpoints currently set }
    function GetModuleBkptCount: Integer;
    { Returns the module name for the index'th Module Load breakpoint }
    function GetModuleBkpt(Index: Integer): string;
    { Creates a new Module load breakpoint }
    procedure NewModuleBreakpoint(const ModuleName: string); overload;
    { Removes a Module Load breakpoint }
    procedure RemoveModuleBreakpoint(const ModuleName: string);
    { Removes a breakpoint }
    procedure RemoveBreakpoint(const Breakpoint: IOTABreakpoint);

    { Registers a debug visualizer }
    procedure RegisterDebugVisualizer(const Visualizer: IOTADebuggerVisualizer);
    { Unregisters a previously registered debug visualizer }
    procedure UnregisterDebugVisualizer(const Visualizer: IOTADebuggerVisualizer);

    { Tells the debugger to process debug events.  This can be used form within
      a loop when waiting for a deferred evaluation to complete }
    procedure ProcessDebugEvents;

    property ModuleBkptCount: Integer read GetModuleBkptCount;
    property ModuleBkpts[Index: Integer]: string read GetModuleBkpt;
  end;

  TOTAFileNotification = (ofnFileOpening, ofnFileOpened, ofnFileClosing,
    ofnDefaultDesktopLoad, ofnDefaultDesktopSave, ofnProjectDesktopLoad,
    ofnProjectDesktopSave, ofnPackageInstalled, ofnPackageUninstalled,
    ofnActiveProjectChanged);

  IOTAIDENotifier = interface(IOTANotifier)
    ['{E052204F-ECE9-11D1-AB19-00C04FB16FB3}']
    { This procedure is called for many various file operations within the
      IDE }
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean);
    { This function is called immediately before the compiler is invoked.
      Set Cancel to True to cancel the compile }
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    { This procedure is called immediately following a compile.  Succeeded
      will be true if the compile was successful }
    procedure AfterCompile(Succeeded: Boolean); overload;
  end;

  IOTAIDENotifier50 = interface(IOTAIDENotifier)
    ['{AC7D29F1-D9A9-11D2-A8C1-00C04FA32F53}']
    { Same as BeforeCompile on IOTAIDENotifier except indicates if the compiler
      was invoked due to a CodeInsight compile }
    procedure BeforeCompile(const Project: IOTAProject; IsCodeInsight: Boolean;
      var Cancel: Boolean); overload;
    { Same as AfterCompile on IOTAIDENotifier except indicates if the compiler
      was invoked due to a CodeInsight compile }
    procedure AfterCompile(Succeeded: Boolean; IsCodeInsight: Boolean); overload;
  end;

  IOTAIDENotifier80 = interface(IOTAIDENotifier50)
    ['{41679BBC-660E-4948-AD80-63C679CB973C}']
    { Same as AfterCompile on IOTAIDENotifier except adds a project, like it
      should have done all along. }
    procedure AfterCompile(const Project: IOTAProject; Succeeded:
      Boolean; IsCodeInsight: Boolean); overload;
  end;

  IOTAGalleryCategory = interface
    ['{CCEA8A72-46BA-4CCF-863C-9718CC06DABF}']
    function GetDisplayName: string;
    function GetIDString: string;
    function GetParent: IOTAGalleryCategory;

    property DisplayName: string read GetDisplayName;
    property IDString: string read GetIDString;
    property Parent: IOTAGalleryCategory read GetParent;
  end;

  { Query BorlandIDEServices for IOTAGalleryCategoryManager }
  IOTAGalleryCategoryManager = interface
    ['{5FAFFE12-E1A4-4286-94F9-A025B3C0BF41}']
    function FindCategory(const IDString: string): IOTAGalleryCategory;
    function AddCategory(const IDString, DisplayName: string;
      IconHandle: Integer = 0): IOTAGalleryCategory; overload;
    function AddCategory(const ParentCategory: IOTAGalleryCategory;
      const IDString, DisplayName: string;
      IconHandle: Integer = 0): IOTAGalleryCategory; overload;
    procedure DeleteCategory(const Category: IOTAGalleryCategory);
  end;

  TWizardState = set of (wsEnabled, wsChecked);

  IOTAWizard = interface(IOTANotifier)
    ['{B75C0CE0-EEA6-11D1-9504-00608CCBF153}']
    { Expert UI strings }
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;

    { Launch the AddIn }
    procedure Execute;
  end;

  { Repository Wizards are the items in the File | New | Other dialog.
    They are used to create new projects, units/forms, etc.
    In order to work properly in the current IDE, each IOTARepositoryWizard
    should also implement IOTARepositoryWizard80 }

  IOTARepositoryWizard = interface(IOTAWizard)
    ['{B75C0CE1-EEA6-11D1-9504-00608CCBF153}']
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
    function GetGlyph: Cardinal;
  end;

  IOTARepositoryWizard60 = interface(IOTARepositoryWizard)
    ['{08FCCD88-3A21-4281-ADC9-62FC034CDD12}']
    { This function should return the appropriate designer affinity for which
      this wizard is applicable.  This will help the File|New|Other... dialog
      filter the appropriate items based on the current project either CLX or
      VCL.  See the dVCL, dCLX, and dAny constants. }
    function GetDesigner: string;

    property Designer: string read GetDesigner;
  end;

  IOTARepositoryWizard80 = interface(IOTARepositoryWizard60)
    ['{D7714D41-BC4A-445E-B695-25A65C2F561E}']
    function GetGalleryCategory: IOTAGalleryCategory;
    function GetPersonality: string;

    { GalleryCategory takes precedence over the result from GetPage.
      If a wizard doesn't implement IOTARepositoryWizard80, it is
      put under the Delphi personality's default section, and creates a
      sub area named by the result of "GetPage". }
    property GalleryCategory: IOTAGalleryCategory read GetGalleryCategory;
    property Personality: string read GetPersonality;
  end;

  IOTAFormWizard = interface(IOTARepositoryWizard)
    ['{36C8BF35-EFFE-11D1-AB1D-00C04FB16FB3}']
  end;

  IOTAFormWizard100 = interface(IOTAFormWizard)
    ['{56D61347-C74D-4BF7-89A9-F422F31CF259}']
    { IsVisible allows the wizard to determine if it should show up in the
      gallery for a given project (if there is no project,
      nil is passed). The wizard must already be the same
      personality as the project.
      The only reason to add this interface is if you may want to return
      false. }
    function IsVisible(Project: IOTAProject): Boolean;
  end;

  IOTAProjectWizard = interface(IOTARepositoryWizard)
    ['{36C8BF36-EFFE-11D1-AB1D-00C04FB16FB3}']
  end;

  IOTAProjectWizard100 = interface(IOTAProjectWizard)
    ['{809D578B-AE79-4CC2-A6ED-D7A8CD24C74D}']
    { IsVisible allows the wizard to determine if it should show up in the
      gallery for a given active project project. The only reason to add this
      interface is if you may want to return false. }
    function IsVisible(Project: IOTAProject): Boolean;
  end;

  IOTAMenuWizard = interface(IOTAWizard)
    ['{B75C0CE2-EEA6-11D1-9504-00608CCBF153}']
    function GetMenuText: string;
  end;

  TWizardRegisterProc = function(const Wizard: IOTAWizard): Boolean;
  TWizardTerminateProc = procedure;
  TWizardInitProc = function(const BorlandIDEServices: IBorlandIDEServices;
    RegisterProc: TWizardRegisterProc;
    var Terminate: TWizardTerminateProc): Boolean stdcall;

  IOTAWizardServices = interface(IUnknown)
    ['{B75C0CE3-EEA6-11D1-9504-00608CCBF153}']
    { Call this to register an IOTAWizard interface }
    function AddWizard(const AWizard: IOTAWizard): Integer;
    { Remove the index'd Wizard }
    procedure RemoveWizard(Index: Integer);
  end;

  IOTAPackageServices = interface(IUnknown)
    ['{26EB0E4D-F97B-11D1-AB27-00C04FB16FB3}']
    { Returns the number of loaded packages }
    function GetPackageCount: Integer;
    { Returns the name of the loaded package }
    function GetPackageName(Index: Integer): string;
    { Returns the number of components registered by this package index }
    function GetComponentCount(PkgIndex: Integer): Integer;
    { Returns the component name of the package index }
    function GetComponentName(PkgIndex, CompIndex: Integer): string;

    property PackageCount: Integer read GetPackageCount;
    property PackageNames[Index: Integer]: string read GetPackageName;
    property ComponentCount[PkgIndex: Integer]: Integer read GetComponentCount;
    property ComponentNames[PkgIndex, CompIndex: Integer]: string read GetComponentName;
  end;

  IOTACustomMessage = interface(IUnknown)
    ['{589BBDA1-F995-11D1-AB27-00C04FB16FB3}']
    { Returns the Column number of the file if the file is given }
    function GetColumnNumber: Integer;
    { Returns a Fully qualified filename if this message line can navigate to a
      file line }
    function GetFileName: string;
    { Returns the Line number of the above file if the above file is given }
    function GetLineNumber: Integer;
    { Returns the Raw line text }
    function GetLineText: string;
    { F1 pressed on this line }
    procedure ShowHelp;

    property ColumnNumber: Integer read GetColumnNumber;
    property FileName: string read GetFilename;
    property LineNumber: Integer read GetLineNumber;
    property LineText: string read GetLineText;
  end;

  IOTACustomMessage50 = interface(IOTACustomMessage)
    ['{B7523AB7-EB81-11D2-AC7B-00C04FB173DC}']
    { Returns the number of child messages the current message has }
    function GetChildCount: Integer;
    { Returns the child message referred to by the Index parameter }
    function GetChild(Index: Integer): IOTACustomMessage50;

    property ChildCount: Integer read GetChildCount;
    property Child[Index: Integer]: IOTACustomMessage50 read GetChild;
  end;

  IOTACustomMessage100 = interface(IOTACustomMessage50)
    ['{824153E3-6336-48BA-805E-1A35E429787E}']
    { Indicates if the line can be navigated to }
    function CanGotoSource(var DefaultHandling: Boolean): Boolean;
    { User selected View Source on this line }
    procedure TrackSource(var DefaultHandling: Boolean);
    { User selected Edit Source on this line }
    procedure GotoSource(var DefaultHandling: Boolean);
  end;

  INTACustomDrawMessage = interface(IOTACustomMessage)
    ['{589BBDA2-F995-11D1-AB27-00C04FB16FB3}']
    procedure Draw(Canvas: TCanvas; const Rect: TRect; Wrap: Boolean);
    function CalcRect(Canvas: TCanvas; MaxWidth: Integer; Wrap: Boolean): TRect;
  end;
  {$IFDEF LINUX}
  {$NODEFINE INTACustomDrawMessage}
  {$ENDIF}

  IOTAMessageServices40 = interface(IUnknown)
    ['{26EB0E4E-F97B-11D1-AB27-00C04FB16FB3}']
    { This method allows the external tool to handle all painting of the
      message in a fashion similar to the "Find in files" messages.  If the
      INTACustomDrawMessage is implemented then all painting is deferred to that
      interface, otherwise the painting is handled internally }
    procedure AddCustomMessage(const CustomMsg: IOTACustomMessage);
    { This method adds a simple title message that simply displays in the
      message view with no other behavior }
    procedure AddTitleMessage(const MessageStr: string);
    { This method adds a line to the message buffer that behaves similar to the
      compiler error message lines.  It allows all the painting and other
      processing to be handled by the IDE.
      PrefixStr is usually a short string that indicates from which tool this
      message originates, for ex. 'Grep tool', which would display as
      '[Grep tool]' }
    procedure AddToolMessage(const FileName, MessageStr, PrefixStr: string;
      LineNumber, ColumnNumber: Integer);
    { Clears all messages from the message buffer }
    procedure ClearAllMessages;
    { Clears all "compiler/linker" messages }
    procedure ClearCompilerMessages;
    { Clears all "Find in files" messages }
    procedure ClearSearchMessages;
    { Clears all the externally defined messages -- from all message groups }
    procedure ClearToolMessages;
  end;

  IOTAMessageServices50 = interface(IOTAMessageServices40)
    ['{3263774B-E959-11D2-AC7B-00C04FB173DC}']
    { See IOTAMessageServices40 for base description.  This method adds an out
      parameter (LineRef) which returns an opaque pointer to the Line object
      created, as well as a Parent.  If Parent is not nil, this method creates a
      new message which is a child message to an existing message (referred to
      by Parent).  A previously returned LineRef pointer should be passed as the
      Parent parameter to create a child message of the Parent. }
    procedure AddToolMessage(const FileName, MessageStr, PrefixStr: string;
      LineNumber, ColumnNumber: Integer; Parent: Pointer; out LineRef: Pointer); overload;
  end;

  IOTAMessageGroup80 = interface(IUnknown)
    ['{233F4508-6022-4DDF-B6D3-D2108BAF80DB}']
    { Returns the name for the group }
    function GetGroupName: string;
    property Name: string read GetGroupName;
  end;

  IOTAMessageGroup90 = interface(IOTAMessageGroup80)
    ['{CF2B68C9-9ED0-461E-A5F4-DFC3B0268A85}']
    { Indicates whether this message group will automatically scroll to show any new
      messages added to it.  Defaults to False }
    function GetAutoScroll: Boolean;
    { Set the AutoScroll state of this message group.  Setting it to True will cause
      the message view to automatically scroll any newly added message into view }
    procedure SetAutoScroll(Value: Boolean);
    property AutoScroll: Boolean read GetAutoScroll write SetAutoScroll;
  end;

  IOTAMessageGroup = interface(IOTAMessageGroup90)
    ['{52A6CDC6-2225-4D3C-AC02-A68C9B19A967}']
    { Indicates whether this message group can be closed by choosing the local menu
      item "Close Tab" in the message view.  Defaults to True }
    function GetCanClose: Boolean;
    { Set the CanClose flag of this message group.  Setting it to True will cause
      the local menu item "Close Tab" to be disabled when it is disaplyed for this
      message group }
    procedure SetCanClose(Value: Boolean);
    property CanClose: Boolean read GetCanClose write SetCanClose;
  end;

  { Interface implemented by a client to receive notifications
    on message groups }
  IOTAMessageNotifier = interface(IOTANotifier)
    ['{FDCB2ED4-B89C-4D00-B0DB-19562951CDBB}']
    { BeforeSave, AfterSave, Destroyed, and Modified are
      currently not called for this notifier, though they
      may be in future releases }
    { Called when a new message group is added }
    procedure MessageGroupAdded(const Group: IOTAMessageGroup);
    { Called when a message group is deleted }
    procedure MessageGroupDeleted(const Group: IOTAMessageGroup);
  end;

  INTAMessageNotifier = interface(IOTAMessageNotifier)
    ['{0AE796BE-B5D7-4830-9159-DEFF82AC076B}']
    { Called when the local menu is brought up on the specified message group.
      Menu is the PopupMenu.  You can insert items into this menu.
      LineRef is an opaque pointer indicating the focused message
      when the menu is brought up.  This can be queried for an IOTACustomMessage
      if you have added such a line to the current message group }
    procedure MessageViewMenuShown(Menu: TPopupMenu; const MessageGroup: IOTAMessageGroup; LineRef: Pointer);
  end;
  {$IFDEF LINUX}
  {$NODEFINE INTAMessageNotifier}
  {$ENDIF}

  IOTAMessageServices60 = interface(IOTAMessageServices50)
    ['{58A40C76-7EC6-41DA-A2EF-4B3AF31D3977}']
    { Call this to register an IOTAMessageNotifier. The result is the index
      to be used when calling RemoveNotifier. If <0 then an error occurred. }
    function AddNotifier(const ANotifier: IOTAMessageNotifier): Integer;
    { Call with the index obtained from AddNotifier }
    procedure RemoveNotifier(Index: Integer);
    { This method allows you to add a new message group to the message view.
      A message group shows up as a distinct tab in the message view in the IDE.
      An interface to the newly added group is returned.  Use this interface
      when calling other methods in the IOTAMessageServices interface.  If you
      hold onto the IOTAMessageGroup reference which is returned, you should
      install an IOTAMessageNotifier and listen for the MessageGroupDeleted
      notification.  When you receive this notification, you should release your
      reference to the indicated IOTAMessageGroup. }
    function AddMessageGroup(const GroupName: string): IOTAMessageGroup;
    { See IOTAMessageServices50 for base description of AddCustomMessage,
      AddTitleMessage and AddToolMessage.  These methods add one parameter:
      MessageGroupIntf indicates which MessageGroup this message belongs to.
      If MessageGroupIntf is nil, the message is added to the "Build" tab. }
    procedure AddCustomMessage(const CustomMsg: IOTACustomMessage;
      const MessageGroupIntf: IOTAMessageGroup); overload;
    procedure AddTitleMessage(const MessageStr: string; const MessageGroupIntf: IOTAMessageGroup); overload;
    procedure AddToolMessage(const FileName, MessageStr, PrefixStr: string;
      LineNumber, ColumnNumber: Integer; Parent: Pointer; out LineRef: Pointer;
      const MessageGroupIntf: IOTAMessageGroup); overload;
    { Clears all messages in the specified group. If MessageGroupIntf is nil,
      all groups are cleared }
    procedure ClearMessageGroup(const MessageGroupIntf: IOTAMessageGroup);
    { Clears all the externally defined messages from the specified group.
      If MessageGroupIntf is nil, all externally defined messages in all
      groups are cleared }
    procedure ClearToolMessages(const MessageGroupIntf: IOTAMessageGroup); overload;
    { Returns the number of message groups currently defined }
    function GetMessageGroupCount: Integer;
    { Returns an interface to the group specified by the index }
    function GetMessageGroup(Index: Integer): IOTAMessageGroup;
    { Returns the specified group }
    function GetGroup(const GroupName: string): IOTAMessageGroup;
    { Tells the IDE to open the message view. If MessageGroupIntf is nil the
      "current" tab is focused. }
    procedure ShowMessageView(const MessageGroupIntf: IOTAMessageGroup);
    { Removes the specified message group.  If MessageGroupIntf is nil then
      this does nothing. }
    procedure RemoveMessageGroup(const MessageGroupIntf: IOTAMessageGroup);
    property MessageGroupCount: Integer read GetMessageGroupCount;
    property MessageGroup[Index: Integer]: IOTAMessageGroup read GetMessageGroup;
  end;

  TOTAMessageKind = (otamkHint, otamkWarn, otamkError, otamkFatal, otamkInfo);

  IOTAMessageServices70 = interface(IOTAMessageServices60)
    ['{B3F7D3A6-D1F7-48A0-8BB0-F49CF60FB815}']
    { This method adds a message to the message window that comes from a
      compiler or other translator. This method allows the build filter to
      indicate to the IDE what kind of message this is, error, warning, etc. }
    procedure AddCompilerMessage(const FileName, MessageStr, ToolName: string;
      Kind: TOTAMessageKind; LineNumber, ColumnNumber: Integer;
      Parent: Pointer; out LineRef: Pointer); overload;
  end;

  IOTAMessageServices80 = interface(IOTAMessageServices70)
    ['{02DD618A-30A0-4DCE-9D04-6B736646FFCB}']
    { This method tells the message window (current tab) to navigate to the next
      or previous message (depending on the GoForward parameter) }
    procedure NextMessage(GoForward: Boolean);
    { This method tells the message window's Build tab to navigate to the next
      or previous message (depending on the GoForward parameter) .  If ErrorsOnly
      is True non-error messages are skipped }
    procedure NextErrorMessage(GoForward: Boolean; ErrorsOnly: Boolean);
    { This message is the same as the method in IOTAMessageServices70.  It
      adds a HelpKeyword parameter.  If the user presses F1 on the message
      added, help will be shown for the specified keyword }
    procedure AddCompilerMessage(const FileName, MessageStr, ToolName: string;
      Kind: TOTAMessageKind; LineNumber, ColumnNumber: Integer; Parent: Pointer;
      out LineRef: Pointer; HelpKeyword: string); overload;
    { This message is the same as the method in IOTAMessageServices70.  It
      adds a HelpContext parameter.  If the user presses F1 on the message
      added, help will be shown for the specified context }
    procedure AddCompilerMessage(const FileName, MessageStr, ToolName: string;
      Kind: TOTAMessageKind; LineNumber, ColumnNumber: Integer; Parent: Pointer;
      out LineRef: Pointer; HelpContext: Integer); overload;
  end;

  IOTAMessageServices = interface(IOTAMessageServices80)
    ['{29E893DB-DD9A-4CEA-B2EE-57532E01A9B9}']
    { Allows a custom message to be added with a parent. Returns: a pointer
      to the current message that can then be used as a parent. }
    function AddCustomMessage(const CustomMsg: IOTACustomMessage;
      Parent: Pointer): Pointer; overload;
    { Same as AddCustomMessage but returns a pointer that allows it to be
      used as a parent of another AddCustomMessage call }
    function AddCustomMessagePtr(const CustomMsg: IOTACustomMessage;
      const MessageGroupIntf: IOTAMessageGroup): Pointer; overload;
    { WideString version AddCompilerMessage }
    procedure AddWideCompilerMessage(const FileName, MessageStr, ToolName: WideString;
      Kind: TOTAMessageKind; LineNumber, ColumnNumber: Integer;
      Parent: Pointer; out LineRef: Pointer); overload;
    procedure AddWideCompilerMessage(const FileName, MessageStr, ToolName: WideString;
      Kind: TOTAMessageKind; LineNumber, ColumnNumber: Integer; Parent: Pointer;
      out LineRef: Pointer; HelpKeyword: WideString); overload;
    procedure AddWideCompilerMessage(const FileName, MessageStr, ToolName: WideString;
      Kind: TOTAMessageKind; LineNumber, ColumnNumber: Integer; Parent: Pointer;
      out LineRef: Pointer; HelpContext: Integer); overload;
    { WideString version AddMessageGroup }
    function AddWideMessageGroup(const GroupName: WideString): IOTAMessageGroup;
    { WideString version AddTitleMessage }
    procedure AddWideTitleMessage(const MessageStr: WideString); overload;
    procedure AddWideTitleMessage(const MessageStr: WideString; const MessageGroupIntf: IOTAMessageGroup); overload;
    { WideString version AddToolMessage }
    procedure AddWideToolMessage(const FileName, MessageStr, PrefixStr: WideString;
      LineNumber, ColumnNumber: Integer); overload;
    procedure AddWideToolMessage(const FileName, MessageStr, PrefixStr: WideString;
      LineNumber, ColumnNumber: Integer; Parent: Pointer; out LineRef: Pointer); overload;
    procedure AddWideToolMessage(const FileName, MessageStr, PrefixStr: WideString;
      LineNumber, ColumnNumber: Integer; Parent: Pointer; out LineRef: Pointer;
     const MessageGroupIntf: IOTAMessageGroup); overload;
    { WideString version GetGroup }
    function GetWideGroup(const GroupName: WideString): IOTAMessageGroup;
  end;

  IOTAEnvironmentOptions = interface(IOTAOptions)
    ['{9C0E91FB-FA5A-11D1-AB28-00C04FB16FB3}']
  end;

  /// <summary>
  /// Allows Help Insight to show documentation information
  /// from the current symbol in the code editor.
  /// Query for it from the IOTAModule. If it isn't present,
  /// then this feature is not present.
  /// </summary>
  IOTAHelpInsight = interface(IDispatch)
    ['{D05FBF46-8468-4934-BF70-7EAE1AB8FEAB}']
    function GetEditorDocInfo(var Line: Integer; var Col: Integer; var Width: Integer): WideString; safecall;
    function GetSymbolDocInfo(const SymbolName: WideString): WideString; safecall;
    function IsEnabled: Boolean; safecall;
  end;

  INTAServices40 = interface(IUnknown)
    ['{3C7F3267-F0BF-11D1-AB1E-00C04FB16FB3}']
    { Adds an image to the IDE's main image list.  Use the return value as an
      image index for an action item added to the IDE's main action list. This
      method is deprecated in favor of the new AddMasked method on the
      INTAServices interface defined below }
    function AddMasked(Image: TBitmap; MaskColor: TColor): Integer; overload;
    { Returns the IDE's Main ActionList component }
    function GetActionList: TCustomActionList;
    { Returns the IDE's Main ImageList component }
    function GetImageList: TCustomImageList;
    { Returns the IDE's Main Menu component }
    function GetMainMenu: TMainMenu;
    { Returns the named Toolbar }
    function GetToolBar(const ToolBarName: string): TToolBar;

    property ActionList: TCustomActionList read GetActionList;
    property ImageList: TCustomImageList read GetImageList;
    property MainMenu: TMainMenu read GetMainMenu;
    property ToolBar[const ToolBarName: string]: TToolBar read GetToolBar;
  end;
  {$IFDEF LINUX}
  {$NODEFINE INTAServices40}
  {$ENDIF}

  INTAServices70 = interface(INTAServices40)
    ['{C17B3DF1-DFE5-11D2-A8C7-00C04FA32F53}']
    { Adds an image to the IDE's main image list.  Use the return value as an
      image index for an action item added to the IDE's main action list.  Ident
      is used to identify dynamically added images that may already exist in the
      imagelist. Since images cannot be safely deleted without causing incorrect
      glyphs to be used for all subsequent images, this allows an add-in to be
      unloaded then reloaded in the same IDE session and still acquire the same
      image index }
    function AddMasked(Image: TBitmap; MaskColor: TColor; const Ident: string): Integer; overload;
  end;
  {$IFDEF LINUX}
  {$NODEFINE INTAServices70}
  {$ENDIF}

  { INTACustomizeToolbarNotifier - When calling INTAServices.CustomizeToolbars,
    provide this interface in order to handle events from the customize dialog.
    These events are backed by default behaviors, so not all methods need to
    do anything except return.
    This interface can also be registered in order to get events for when
    *anyone* calls INTAServices.CustomizeToolbars.  In this case, this notifier
    will only be called *after* the caller of CustomizeToolbar's notifier is
    called.  Not all events will be called as noted below. }

  INTACustomizeToolbarNotifier = interface(IOTANotifier)
    ['{34F2C852-0B8C-4352-908D-94038E79CC0D}']
    { Create a TToolButton or even some other control that will be placed on the
      toolbar when dropped.  This control should be created based on the given
      action.  The control doesn't have to be a button.  For instance it may
      be a Combobox.  The first notifier that returns with Button <> nil stops
      the calling of subsequent notifiers. }
    procedure CreateButton(AOwner: TComponent; var Button: TControl;
      Action: TBasicAction);
    { This event is called in order to determine what the display name of the
      action should be and whether or not it should be displayed at all. }
    procedure FilterAction(Action: TBasicAction; ViewingAllCommands: Boolean;
      var DisplayName: string; var Display: Boolean; var Handled: Boolean);
    { This event is called in order to determine the display name of a category or
      whether or not the category is even displayed at all }
    procedure FilterCategory(var Category: string; var Display: Boolean;
      var Handled: Boolean);
    { This event is called in response to the user pressing reset in the
      customize dialog.  Note this event is *not* called for general listeners. }
    procedure ResetToolbar(var Toolbar: TWinControl);
    { Called when the user checks or unchecks the check box next to the given
      toolbar in the list of editable toolbars.  Note this event is *not* called
      for general listeners. }
    procedure ShowToolbar(Toolbar: TWinControl; Show: Boolean);
    { Called whenever any toolbar actually got modified during the customize
      operation.  Callers of CustomizeToolbars, will get this event called first,
      and should stream the state of the toolbar so that other listeners can
      reload their state.  General listeners should check the toolbar name and
      reload their toolbar if this is a like-named instance and this is not the
      instance they just customized. }
    procedure ToolbarModified(Toolbar: TWinControl);
  end;

  { INTAToolbarStreamNotifier - Register this interface to receive events
    related reading and writing of a toolbar to a stream. }

  INTAToolbarStreamNotifier = interface(IOTANotifier)
    ['{A2D9F2F7-815E-4E5B-BD83-2BD4A57A45E1}']
    { This method is called from WriteToolbar immediately after the toolbar
      was saved to the stream. }
    procedure AfterSave(Toolbar: TWinControl);
    { This method is called from WriteToolbar just before the toolbar is saved
      to the stream. }
    procedure BeforeSave(Toolbar: TWinControl);
    { ToolbarLoaded is called from within Readtoolbar immediately after the
      toolbar was loaded from the stream.  If the toolbar was not actually
      constructed from the stream, this method is *not* called. }
    procedure ToolbarLoaded(Toolbar: TWinControl);
  end;

  { INTAReadToolbarNotifier - Register this interface to get certain VCL
    streaming events so that when toolbars are streamed in, events on the
    components can be re-attached to the proper instance. See information
    on TReader for the explanation of these events. }
  INTAReadToolbarNotifier = interface(IOTANotifier)
    ['{748F68BB-599C-4BE4-83A3-EEEBD920B6EE}']
    procedure FindMethodInstance(Reader: TReader; const MethodName: string;
      var Method: TMethod; var Error: Boolean);
    procedure SetName(Reader: TReader; Component: TComponent; var Name: string; var Handled: Boolean);
    procedure ReadError(Reader: TReader; const Message: string; var Handled: Boolean);
  end;

  { INTAWriteToolberNotifier - Register this interface to get certain VCL
    streaming events so that when the toolbars are streamed out, events on the
    components can have names associated with other objects }
  INTAWriteToolbarNotifier = interface(IOTANotifier)
    ['{89B84491-A034-4097-AD64-4BAA5211BF2E}']
    procedure FindMethodName(Writer: TWriter; Method: TMethod; var MethodName: string);
  end;

  INTAServices90 = interface(INTAServices70)
    ['{89160C3A-8EF4-4D2E-8FD5-D8492F61DB3E}']
    {! AddImages takes all the images from the given image list and adds them to the
       main application imagelist.  It also creates an internal mapping array from the
       original image indices to the new indices in the main imagelist.  This
       mapping is used by AddActionMenu to remap the ImageIndex property of the
       action object to the new ImageIndex.  This should be the first method
       called when adding actions and menu items to the main application window.
       The return value is the first index in the main application image list of
       the first image in the source list. Call this function with an nil
       image list to clear the internal mapping array. }
    function AddImages(AImages: TCustomImageList): Integer; overload;
    {! AddActionMenu takes an action item, a menu item, and a menu item component name
       and inserts the action in to the main action list and the menu item into the
       menu either preceding or following the named menu component.  If the action
       component has an ImageIndex > -1, then the mapping table created by the
       previous call to AddImages above is used to determine the new value for
       ImageIndex.  NewAction can be nil, in which case only the menu item is
       added. Likewise, NewMenu can be nil, in which case the Name param is
       ignored and only the action is added to the main action list. If Name
       cannot be found, an exception is raised.  If the ImageIndex of NewAction
       is out of range, then it is set to -1. }
    procedure AddActionMenu(const Name: string; NewAction: TCustomAction;
      NewItem: TMenuItem; InsertAfter: Boolean = True; InsertAsChild: Boolean = False);
    {! NewToolBar creates a new toolbar with the given name and caption.
       If the ReferenceToolBar parameter is specified, it is used as a reference point
       for insertion based on the InsertBefore parameter.  If InsertBefore is True, then
       the new toolbar is inserted physically before the reference, else it is after.
       if ReferenceToolBar is not specified, then the toolbar is inserted into a
       position determined by the IDE. }
    function NewToolbar(const Name, Caption: string;
      const ReferenceToolBar: string = '';
      InsertBefore: Boolean = False): TToolbar;
    {! AddToolButton creates a new toolbutton on the named toolbar using the given
       Action component.  In order for the user to be able to add and remove this
       toolbutton, an Action *must* be specified.  otherwise the user may remove
       the button, never to return until the toolbar config entries in the registry
       are deleted and the toolbars are reset to the original configuration. If
       IsDivider is True, then Action is ignored since a divider toolbutton is
       created. If you wish the toolbutton to have a dropdown menu, then owner-
       ship of that menu *must* be transferred to the owner of the toolbutton. }
    function AddToolButton(const ToolBarName, ButtonName: string;
      AAction: TCustomAction; const IsDivider: Boolean = False;
      const ReferenceButton: string = ''; InsertBefore: Boolean = False): TControl;
    {! UpdateMenuAccelerators causes the IDE to reset all the assigned accelerator
       keys to the associated menu items.  This is the accelerators as defined in
       the current keymap }
    procedure UpdateMenuAccelerators(Menu: TMenu);
    {! ReadToolbar reads the configuration of the given toolbar from the
       registry and recreates it if necessary. If SubKey is specified, it will
       attempt to obtain a stream from that key first and if not found, then
       will get the stream from the main toolbar key.  Use Subkey to read
       view-specific version of a toolbar. You can also optionally pass in a
       TStream object if you wish to control the actual storage of the stream
       itself. Set DefaultToolbar to true in order to read one of the global
       toolbars from the "toolbar reset" storage.  This is the default
       configuration of the toolbar. }
    procedure ReadToolbar(AOwner: TComponent; AParent: TWinControl; const AName: string;
      var AToolBar: TWinControl; const ASubKey: string = ''; AStream: TStream = nil;
      DefaultToolbar: Boolean = False);
    {! WriteToolbar will take the given toolbar and write it out to the registry
       under the subkey name if specified.  Use SubKey to write view-specific
       versions of a toolbar. }
    procedure WriteToolbar(AToolbar: TWinControl; const AName: string = '';
      const ASubkey: string = ''; AStream: TStream = nil);
    {! CustomizeToolbars will open the toolbar customize dialog and set the given
       toolbars into customize mode.  The INTACustomizeToolbarNotifier interface
       is used to handler certain events during the customizing process.  If
       ActionList is specified, then only the actions in that action list can
       be used to customize the toolbar.  If not specified, =nil, then the IDE's
       global action list is used.  The return value is the customize dialog
       component.  You can add a FreeNotification in order to know when the user
       closes the dialog and customization is complete. }
    function CustomizeToolbar(const AToolbars: array of TWinControl;
      const ANotifier: INTACustomizeToolbarNotifier; AButtonOwner: TComponent = nil;
      AActionList: TCustomActionList = nil; AButtonsOnly: Boolean = True): TComponent;
    {! Call CloseCustomize when it is needed to forcibly terminate the customize
       mode.  For instance if the view being customized is destroyed or hidden,
       this procedure may be called to terminate customization. }
    procedure CloseCustomize;
    {! Call ToolbarModified if you wish to notify all interested parties that
       a toolbar was modified by means other than calling CustomizeToolbar.  For
       instance, when the toolbar is on a TControlBar and the toolbar band was
       repositioned.  This will cause all registered INTACustomizeToolbarNotifier.ToolbarModified
       events to be called. }
    procedure ToolbarModified(AToolbar: TWinControl);
    {! RegisterToolbarNotifier registers the given INTACustomizeToolbarNotifier
       in order to receive certain events related to toolbars and customizing
       them.  The most often used event will probably be the ToolbarModified
       event since that is how other views can know when a particular named
       toolbar is customized. }
    function RegisterToolbarNotifier(const ANotifier: IOTANotifier): Integer;
    procedure UnregisterToolbarNotifier(Index: Integer);
    {! MenuBegin/EndUpdate allows the caller to control how often the main
       menu will be updated.  If many changes to the main menu are made at
       one time performance can be improved by using these methods. }
    procedure MenuBeginUpdate;
    procedure MenuEndUpdate;
  end;
  {$IFDEF LINUX}
  {$NODEFINE INTAServices90}
  {$ENDIF}

  INTAServices120 = interface(INTAServices90)
    ['{26B056B7-FB49-41BC-A8C2-151DE1EBF465}']
    {! AddImages takes all the images from the given image list and adds them to the
       main application imagelist.  It also creates an internal mapping array from the
       original image indices to the new indices in the main imagelist.  This
       mapping is used by AddActionMenu to remap the ImageIndex property of the
       action object to the new ImageIndex.  This should be the first method
       called when adding actions and menu items to the main application window.
       The return value is the first index in the main application image list of
       the first image in the source list. Call this function with an nil
       image list to clear the internal mapping array. Unlike the AddImages function from
       the ancestor interface, this version takes an Ident that allows the same base index
       to be re-used.  This is useful when the IDE implements demand-loading of
       personalities so that the images will only get registered once and the same image
       indices can be used.}
    function AddImages(AImages: TCustomImageList; const Ident: string): Integer; overload;
  end;
  {$IFDEF LINUX}
  {$NODEFINE INTAServices120}
  {$ENDIF}

  INTACustomDockableForm = interface(IUnknown)
    ['{F8EF756F-4E95-4F1F-B229-A3DEF7BCC350}']
    { Returns the Caption for the Dockable Form }
    function GetCaption: string;
    { Returns a unique identifier for this form.  This should not be translated.
      This identifier is used as the section name when saving information for
      this form in the desktop state file }
    function GetIdentifier: string;
    { Returns the class of the frame that you want embedded in the dockable form }
    function GetFrameClass: TCustomFrameClass;
    { Called when an instance of the specified frame class is created }
    procedure FrameCreated(AFrame: TCustomFrame);
    { Returns an action list that is used to populate the form's context menu.
      By default the context menu will have 2 items that are common to all
      dockable forms in the IDE: "Stay on top" and "Dockable".  If the form
      has a toolbar, there will also be a "Toolbar" menu item.  If this
      function returns a non-nil action list, the items in the action list will
      be added to the menu (above the default items).  To specify sub-menus, use
      categories for the actions contained in the Action List.  Any action that
      has a Category set, will appear on a sub-menu in the context menu.  The
      Caption of the Parent menu will be the Category name. }
    function GetMenuActionList: TCustomActionList;
    { Returns an image list that contains the images associated with the action
      list returned by GetMenuActionList }
    function GetMenuImageList: TCustomImageList;
    { Called when the popup menu is about to be shown.  This allows further
      customization beyond just adding items from an Action List }
    procedure CustomizePopupMenu(PopupMenu: TPopupMenu);
    { Returns an action list that is used to populate a toolbar on the form.  If
      nil is returned, then the dockable form will not have a toolbar.  Items in
      the Action List that have '-' as the caption will be added to the toolbar
      as a separator }
    function GetToolBarActionList: TCustomActionList;
    { Returns an image list that contains the images associated with the action
      list returned by GetToolbarActionList }
    function GetToolBarImageList: TCustomImageList;
    { Called after the toolbar has been populated with the Action List returned
      from GetToolbarActionList.  This allows further customization beyond just
      adding items from an Action List }
    procedure CustomizeToolBar(ToolBar: TToolBar);
    { Called when state for this form is saved to a desktop file.  The Section
      paramter is passed in for convenience, but it should match the string
      returned by GetIdentifier.  This is only called for INTACustomDockableForm
      instances that have been registered using INTAServices.RegisterDockableForm.
      IsProject indicates whether the desktop being saved is a project desktop
      (as opposed to a dekstop state) }
    procedure SaveWindowState(Desktop: TCustomIniFile; const Section: string; IsProject: Boolean);
    { Called when state for this form is loaded from a desktop file.  The
      Section paramter is passed in for convenience, but it should match the
      string returned by GetIdentifier.  This is only called for
      INTACustomDockableForm instances that have been registered using
      INTAServices.RegisterDockableForm }
    procedure LoadWindowState(Desktop: TCustomIniFile; const Section: string);
    { Allows the form to control the enabled state of the clipboard commands on
      the IDE's "Edit" menu when this view is active }
    function GetEditState: TEditState;
    { Called when the user uses one of the clipboard commands on the IDE's "Edit"
      menu }
    function EditAction(Action: TEditAction): Boolean;

    property Caption: string read GetCaption;
    property Identifier: string read Getidentifier;
    property FrameClass: TCustomFrameClass read GetFrameClass;
    property MenuActionList: TCustomActionList read GetMenuActionList;
    property MenuImageList: TCustomImageList read GetMenuImageList;
    property ToolbarActionList: TCustomActionList read GetToolbarActionList;
    property ToolbarImageList: TCustomImageList read GetToolbarImageList;
  end;
  {$IFDEF LINUX}
  {$NODEFINE INTACustomDockableForm}
  {$ENDIF}

  INTAServices = interface(INTAServices120)
    ['{8209041F-F37F-4570-88B8-6C310FFFF81A}']
    { Registers an INTACustomDockableForm with the IDE.  Registration is not
      required, but doing so will allow the form to participate in saving to and
      loading from a desktop state.  To ensure proper handling of the desktop
      state that is loaded during IDE startup, be sure to call
      RegisterDockableForm from within a "Register" procedure in your package.
      If you do not want your form to participate in desktop saving, you can
      call CreateDockableForm directly without first registering your form }
    procedure RegisterDockableForm(const CustomDockableForm: INTACustomDockableForm);
    { Unregisters a previously registered INTACustomDockableForm }
    procedure UnregisterDockableForm(const CustomDockableForm: INTACustomDockableForm);
    { Creates and displays an INTACustomDockableForm.  Returns the form instance
      that was created. }
    function CreateDockableForm(const CustomDockableForm: INTACustomDockableForm): TCustomForm;
  end;
  {$IFDEF LINUX}
  {$NODEFINE INTAServices}
  {$ENDIF}

  IOTAServices50 = interface(IUnknown)
    ['{7FD1CE91-E053-11D1-AB0B-00C04FB16FB3}']
    { General IDE event notifications }
    function AddNotifier(const Notifier: IOTAIDENotifier): Integer;
    { Remove the index'd notifier }
    procedure RemoveNotifier(Index: Integer);
    { Configuration Access }
    function GetBaseRegistryKey: string;
    { Returns the product Identifier, 'C++Builder' or 'Delphi' }
    function GetProductIdentifier: string;
{$IFDEF MSWINDOWS}
    { Main application handle }
    function GetParentHandle: HWND;
{$ENDIF}
    { Return an interface to the IDE environment options }
    function GetEnvironmentOptions: IOTAEnvironmentOptions;
  end;

  IOTAServices60 = interface(IOTAServices50)
    ['{577ECE00-59EE-4F21-8190-9FD8A45FE550}']
    { Return 'xfm' for CLX designer and 'dfm' for VCL designer }
    function GetActiveDesignerType: string;
  end;

  IOTAServices70 = interface(IOTAServices60)
    ['{0044BB24-425D-D611-9CF1-00C04FA06AFC}']
    { Returns the location of the root installation directory of the IDE }
    function GetRootDirectory: string;
    { Returns the location of the bin directory }
    function GetBinDirectory: string;
    { Returns the location of the template/object-repository directory }
    function GetTemplateDirectory: string;
  end;

  IOTAServices100 = interface(IOTAServices70)
    ['{33B33186-3CEC-4624-970E-417A8FE14089}']
    { Returns the location of the Application Data directory for the
      current user and product. }
    function GetApplicationDataDirectory: string;
  end;

  IOTAServices110 = interface(IOTAServices100)
    ['{17A48937-2C9C-4543-AB6D-2CF13BAE544B}']
    { Returns the location of the Local Application Data directory for the
      current user and product. }
    function GetLocalApplicationDataDirectory: string;
  end;

  IOTAServices = interface(IOTAServices110)
    ['{80E56DFA-82B2-425A-921E-8E5ED6164A11}']
    { Returns the IDE's preferred UI languages as comma separated string. }
    function GetIDEPreferredUILanguages: string;
  end;

  IOTABufferOptions60 = interface(IUnknown)
    ['{8C748540-C6C1-11D2-8139-00609792F134}']
    function GetAutoIndent: Boolean;
    function GetBackspaceUnindents: Boolean;
    function GetCreateBackupFile: Boolean;
    function GetCursorThroughTabs: Boolean;
    function GetInsertMode: Boolean;
    function GetGroupUndo: Boolean;
    function GetKeepTrailingBlanks: Boolean;
    function GetLeftGutterWidth: Integer;
    function GetRightMargin: Integer;
    function GetOverwriteBlocks: Boolean;
    function GetPersistentBlocks: Boolean;
    function GetPreserveLineEnds: Boolean;
    function GetSmartTab: Boolean;
    function GetSyntaxHighlight: Boolean;
    function GetTabStops: string;
    function GetUndoAfterSave: Boolean;
    function GetUndoLimit: Integer;
    function GetUseTabCharacter: Boolean;
    procedure SetAutoIndent(Value: Boolean);
    procedure SetBackspaceUnindents(Value: Boolean);
    procedure SetCreateBackupFile(Value: Boolean);
    procedure SetCursorThroughTabs(Value: Boolean);
    procedure SetInsertMode(Value: Boolean);
    procedure SetGroupUndo(Value: Boolean);
    procedure SetKeepTrailingBlanks(Value: Boolean);
    procedure SetLeftGutterWidth(Value: Integer);
    procedure SetRightMargin(Value: Integer);
    procedure SetOverwriteBlocks(Value: Boolean);
    procedure SetPersistentBlocks(Value: Boolean);
    procedure SetPreserveLineEnds(Value: Boolean);
    procedure SetSmartTab(Value: Boolean);
    procedure SetSyntaxHighlight(Value: Boolean);
    procedure SetTabStops(const Value: string);
    procedure SetUndoAfterSave(Value: Boolean);
    procedure SetUndoLimit(Value: Integer);
    procedure SetUseTabCharacter(Value: Boolean);

    property AutoIndent: Boolean read GetAutoIndent write SetAutoIndent;
    property BackspaceUnindents: Boolean read GetBackspaceUnindents write SetBackspaceUnindents;
    property CreateBackupFile: Boolean read GetCreateBackupFile write SetCreateBackupFile;
    property CursorThroughTabs: Boolean read GetCursorThroughTabs write SetCursorThroughTabs;
    property InsertMode: Boolean read GetInsertMode write SetInsertMode;
    property GroupUndo: Boolean read GetGroupUndo write SetGroupUndo;
    property KeepTrailingBlanks: Boolean read GetKeepTrailingBlanks write SetKeepTrailingBlanks;
    property LeftGutterWidth: Integer read GetLeftGutterWidth write SetLeftGutterWidth;
    property RightMargin: Integer read GetRightMargin write SetRightMargin;
    property OverwriteBlocks: Boolean read GetOverwriteBlocks write SetOverwriteBlocks;
    property PersistentBlocks: Boolean read GetPersistentBlocks write SetPersistentBlocks;
    property PreserveLineEnds: Boolean read GetPreserveLineEnds write SetPreserveLineEnds;
    property SmartTab: Boolean read GetSmartTab write SetSmartTab;
    property SyntaxHighlight: Boolean read GetSyntaxHighlight write SetSyntaxHighlight;
    property TabStops: string read GetTabStops write SetTabStops;
    property UndoAfterSave: Boolean read GetUndoAfterSave write SetUndoAfterSave;
    property UndoLimit: Integer read GetUndoLimit write SetUndoLimit;
    property UseTabCharacter: Boolean read GetuseTabCharacter write SetUseTabCharacter;
  end;

  IOTABufferOptions70 = interface(IOTABufferOptions60)
    ['{F8F13ACE-1124-408F-934F-8E92DA135424}']
    function GetShowSpace: Boolean;
    function GetShowTab: Boolean;
    procedure SetShowSpace(Value: Boolean);
    procedure SetShowTab(Value: Boolean);
    property ShowSpace: Boolean read GetShowSpace write SetShowSpace;
    property ShowTab: Boolean read GetShowTab write SetShowTab;
  end;

  IOTABufferOptions = interface(IOTABufferOptions70)
    ['{38E7F12C-7682-40E9-B13B-9C2E36BDBAFC}']
    function GetHighlightCurrentLine: Boolean;
    function GetShowLineBreaks: Boolean;
    procedure SetHighlightCurrentLine(Value: Boolean);
    procedure SetShowLineBreaks(Value: Boolean);
    property HighlightCurrentLine: Boolean read GetHighlightCurrentLine write SetHighlightCurrentLine;
    property ShowLineBreaks: Boolean read GetShowLineBreaks write SetShowLineBreaks;
  end;

  IOTAEditLineNotifier = interface(IOTANotifier)
    ['{D9D48F50-E6CC-11D2-ABE8-00C04FB16FB3}']
    procedure LineChanged(OldLine, NewLine: Integer; Data: Integer);
  end;

  IOTAEditLineTracker = interface(IUnknown)
    ['{D9D48F4F-E6CC-11D2-ABE8-00C04FB16FB3}']
    function AddNotifier(const Notifier: IOTAEditLineNotifier): Integer;
     
    procedure AddLine(Line: Integer; Data: Integer);
    procedure Delete(Index: Integer);
    function GetCount: Integer;
    function GetData(Index: Integer): Integer;
    function GetEditBuffer: IOTAEditBuffer;
    function GetLineNum(Index: Integer): Integer;
    function IndexOfLine(Line: Integer): Integer;
    function IndexOfData(Data: Integer): Integer;
    procedure RemoveNotifier(Index: Integer);
    procedure SetData(Index: Integer; Value: Integer);
    procedure SetLineNum(Index: Integer; Value: Integer);

    property Count: Integer read GetCount;
    property Data[Index: Integer]: Integer read GetData write SetData;
    property LineNum[Index: Integer]: Integer read GetLineNum write SetLineNum;
  end;

  IOTAEditBuffer60 = interface(IOTASourceEditor)
    ['{9C510460-C7BC-11D2-9AEB-00A02457621F}']
    procedure ClearUndo;
    function GetBufferOptions: IOTABufferOptions;
    function GetCurrentDate: TDateTime;
    function GetEditBlock: IOTAEditBlock;
    function GetEditLineTracker: IOTAEditLineTracker;
    function GetEditPosition: IOTAEditPosition;
    function GetInitialDate: TDateTime;
    function GetIsModified: Boolean;
    function GetIsReadOnly: Boolean;
    function GetTopView: IOTAEditView;
    function Print: Boolean;
    function Redo: Boolean;
    procedure SetIsReadOnly(Value: Boolean);
    function Undo: Boolean;

    property BufferOptions: IOTABufferOptions read GetBufferOptions;
    property EditBlock: IOTAEditBlock read GetEditBlock;
    property EditPosition: IOTAEditPosition read GetEditPosition;
    property IsModified: Boolean read GetIsModified;
    property IsReadOnly: Boolean read GetIsReadOnly write SetIsReadOnly;
    property TopView: IOTAEditView read GetTopView;
  end;

  IOTAEditBuffer = interface(IOTAEditBuffer60)
    ['{EB6465CE-D901-43C4-AB69-240A7400B9AA}']
    { GetEditOptions returns the edit options for this current
      edit buffer. }
    function GetEditOptions: IOTAEditOptions;
    property EditOptions: IOTAEditOptions read GetEditOptions;
  end;

  IOTAEditBufferIterator = interface(IUnknown)
    ['{8ECB33AA-D0BD-11D2-ABD6-00C04FB16FB3}']
    function GetCount: Integer;
    function GetEditBuffer(Index: Integer): IOTAEditBuffer;

    property Count: Integer read GetCount;
    property EditBuffers[Index: Integer]: IOTAEditBuffer read GetEditBuffer;
  end;

  PKeyBindingRec = ^TKeyBindingRec;
  TKeyBindingRec = record
    KeyCode: TShortCut;
    KeyProc: TKeyBindingProc;
    Context: Pointer;
    Next: Integer;
    Reserved: Integer;
  end;

  IOTAKeyContext = interface(IUnknown)
    ['{3E7790CB-D2BB-11D2-ABD8-00C04FB16FB3}']
    function GetContext: Pointer;
    function GetEditBuffer: IOTAEditBuffer;
    function GetKeyboardServices: IOTAKeyboardServices;
    function GetKeyBindingRec(out BindingRec: TKeyBindingRec): Boolean;

    property Context: Pointer read GetContext;
    property EditBuffer: IOTAEditBuffer read GetEditBuffer;
    property KeyboardServices: IOTAKeyboardServices read GetKeyboardServices;
  end;

  IOTARecord = interface(IUnknown)
    ['{F8CAF8D6-D263-11D2-ABD8-00C04FB16FB3}']
    procedure Append(const Keys: array of TShortCut); overload;
    procedure Append(const CmdName: string; IsKeys: Boolean = True); overload;
    procedure Append(const ARecord: IOTARecord); overload;
    procedure Clear;
    function GetIsPaused: Boolean;
    function GetIsPlaying: Boolean;
    function GetIsRecording: Boolean;
    function GetName: string;
    procedure ReadFromStream(const Stream: IStream);
    procedure SetName(const Value: string);
    procedure WriteToStream(const Stream: IStream);

    property IsPaused: Boolean read GetIsPaused;
    property IsPlaying: Boolean read GetIsPlaying;
    property IsRecording: Boolean read GetIsRecording;
    property Name: string read GetName write SetName;
  end;

  TKeyBindingFlags = Integer;

  IOTAKeyBindingServices = interface(IUnknown)
    ['{F8CAF8D8-D263-11D2-ABD8-00C04FB16FB3}']
    function AddKeyBinding(const Keys: array of TShortCut; KeyProc: TKeyBindingProc;
      Context: Pointer; Flags: TKeyBindingFlags = kfImplicitShift or
      kfImplicitModifier or kfImplicitKeypad; const Keyboard: string = '';
      const MenuItemName: string = ''): Boolean;
    function AddMenuCommand(const Command: string; KeyProc: TKeyBindingProc;
      Context: Pointer): Boolean;
    procedure SetDefaultKeyProc(KeyProc: TKeyBindingProc; Context: Pointer;
      const Keyboard: string = '');
  end;

  IOTAKeyboardBinding = interface(IOTANotifier)
    ['{F8CAF8D7-D263-11D2-ABD8-00C04FB16FB3}']
    function GetBindingType: TBindingType;
    function GetDisplayName: string;
    function GetName: string;
    procedure BindKeyboard(const BindingServices: IOTAKeyBindingServices);

    property BindingType: TBindingType read GetBindingType;
    property DisplayName: string read GetDisplayName;
    property Name: string read GetName;
  end;

  IOTAKeyboardServices = interface(IUnknown)
    ['{F8CAF8D5-D263-11D2-ABD8-00C04FB16FB3}']
    function AddKeyboardBinding(const KeyBinding: IOTAKeyboardBinding): Integer;
    function GetCurrentPlayback: IOTARecord;
    function GetCurrentRecord: IOTARecord;
    function GetEditorServices: IOTAEditorServices;
    function GetKeysProcessed: LongWord;
    function NewRecordObject(out ARecord: IOTARecord): Boolean;
    procedure PausePlayback;
    procedure PauseRecord;
    procedure PopKeyboard(const Keyboard: string);
    function PushKeyboard(const Keyboard: string): string;
    procedure RestartKeyboardServices;
    procedure ResumePlayback;
    procedure ResumeRecord;
    procedure RemoveKeyboardBinding(Index: Integer);
    procedure SetPlaybackObject(const ARecord: IOTARecord);
    procedure SetRecordObject(const ARecord: IOTARecord);
    function LookupKeyBinding(const Keys: array of TShortCut;
      out BindingRec: TKeyBindingRec; const KeyBoard: string = ''): Boolean;
    function GetNextBindingRec(var BindingRec: TKeyBindingRec): Boolean;
    function CallKeyBindingProc(const BindingRec: TKeyBindingRec): TKeyBindingResult;

    property CurrentPlayback: IOTARecord read GetCurrentPlayback;
    property CurrentRecord: IOTARecord read GetCurrentRecord;
    property EditorServices: IOTAEditorServices read GetEditorServices;
    property KeysProcessed: LongWord read GetKeysProcessed;
  end;

  { Allows reporting of keypresses and handlers to the message view.
    BorlandIDEServices implements this interface. }
  IOTAKeyboardDiagnostics = interface(IUnknown)
    ['{AEFC65F1-2504-11D3-AC25-00C04FB16FB3}']
    function GetKeyTracing: Boolean;
    procedure SetKeyTracing(Value: Boolean);

    property KeyTracing: Boolean read GetKeyTracing write SetKeyTracing;
  end;

  IOTASpeedSetting = interface(IOTANotifier)
    ['{B5CDCE07-E093-11D2-ABE2-00C04FB16FB3}']
    function GetDisplayName: string;
    function GetName: string;
    { ExecuteSetting is passed the current editor options.
      If you want the speed setting to apply to all IOTAEditOptions, then
      you must enumerate through them using the IOTAEditorServices. }
    procedure ExecuteSetting(const EditOptions: IOTAEditOptions);
    property DisplayName: string read GetDisplayName;
    property Name: string read GetName;
  end;

  IOTAEditOptions60 = interface(IUnknown)
    ['{487BEA91-DBC0-11D2-ABDE-00C04FB16FB3}']
    function AddSpeedSetting(const SpeedSetting: IOTASpeedSetting): Integer;

    procedure BeginUpdate;
    procedure EndUpdate;
    function GetBlockIndent: Integer;
    function GetBufferOptions: IOTABufferOptions;
    function GetFontName: string;
    function GetFontSize: Integer;
    function GetForceCutCopyEnabled: Boolean;
    function GetSpeedSettingCount: Integer;
    function GetSpeedSetting(Index: Integer): IOTASpeedSetting;
    { GetSyntaxHighlightTypes is deprecated. Use SyntaxHighlighter. }
    function GetSyntaxHighlightTypes(Index: TOTASyntaxHighlighter): string; deprecated;
    function GetUseBriefCursorShapes: Boolean;
    function GetUseBriefRegularExpressions: Boolean;
    procedure RemoveSpeedSetting(Index: Integer);
    procedure SetBlockIndent(Value: Integer);
    procedure SetFontName(const Value: string);
    procedure SetFontSize(Value: Integer);
    procedure SetForceCutCopyEnabled(Value: Boolean);
    procedure SetSpeedSetting(const Name: string);
    { SetSyntaxHighlightTypes is deprecated. Use SyntaxHighlighter. }
    procedure SetSyntaxHighlightTypes(Index: TOTASyntaxHighlighter; const Value: string); deprecated;
    procedure SetUseBriefCursorShapes(Value: Boolean);
    procedure SetUseBriefRegularExpressions(Value: Boolean);
    property BlockIndent: Integer read GetBlockIndent write SetBlockIndent;
    property BufferOptions: IOTABufferOptions read GetBufferOptions;
    property FontName: string read GetFontName write SetFontName;
    property FontSize: Integer read GetFontSize write SetFontSize;
    property ForceCutCopyEnabled: Boolean read GetForceCutCopyEnabled
      write SetForceCutCopyEnabled;
    property SpeedSettingCount: Integer read GetSpeedSettingCount;
    property SpeedSettings[Index: Integer]: IOTASpeedSetting read GetSpeedSetting;
    { SyntaxHighlightTypes is deprecated. Use SyntaxHighlighter. }
    property SyntaxHighlightTypes[Index: TOTASyntaxHighlighter]: string
      read GetSyntaxHighlightTypes write SetSyntaxHighlightTypes;
    property UseBriefCursorShapes: Boolean read GetUseBriefCursorShapes
      write SetUseBriefCursorShapes;
    property UseBriefRegularExpressions: Boolean read GetUseBriefRegularExpressions
      write SetUseBriefRegularExpressions;
  end;

  { IOTAEditOptions }
  { Certain options are now associated with particular file types,
    as seen in the Editor Options. Use the IOTAEditorServices to
    get options for a particular internal id, passing one of the
    cDefEd consts from the top of this unit.
    You may also add your own editor options using the IOTAEditorServices. }
  IOTAEditOptions = interface(IOTAEditOptions60)
    ['{02999EF7-669C-406B-8E14-4FE8B27542B8}']
    function GetExtensions: string;
    function GetOptionsName: string;
    function GetOptionsIDString: string;
    function GetSyntaxHighlighter: IOTAHighlighter;
    function GetOptionsIndex: Integer;
    procedure SetExtensions(const Value: string);
    procedure SetOptionsName(const Value: string);
    procedure SetSyntaxHighlighter(const Value: IOTAHighlighter);

    property Extensions: string read GetExtensions write SetExtensions;
    property OptionsName: string read GetOptionsName write SetOptionsName;
    property IDString: string read GetOptionsIDString;
    property SyntaxHighlighter: IOTAHighlighter read GetSyntaxHighlighter
      write SetSyntaxHighlighter;
    property OptionsIndex: Integer read GetOptionsIndex;
  end;

  IOTAEditorExplorerPersonalityTrait = interface(IOTANotifier)
    ['{76B0B1C4-A87D-473D-B6F5-CADCDEF78F30}']
    procedure ViewModified;
    procedure DoClassComplete;
    procedure DoClassNavigate;
  end;

  IOTAEditorServices60 = interface(IUnknown)
    ['{C2812BA7-C48D-11D2-9AE8-00A02457621F}']
    { EditOptions/GetEditOptions now return the edit options for the active
      editor, which will be for whatever file type it is editing. }
    function GetEditOptions: IOTAEditOptions;
    function GetEditBufferIterator(out Iterator: IOTAEditBufferIterator): Boolean;
    function GetKeyboardServices: IOTAKeyboardServices;
    function GetTopBuffer: IOTAEditBuffer;
    function GetTopView: IOTAEditView;

    property EditOptions: IOTAEditOptions read GetEditOptions;
    property KeyboardServices: IOTAKeyboardServices read GetKeyboardServices;
    property TopBuffer: IOTAEditBuffer read GetTopBuffer;
    property TopView: IOTAEditView read GetTopView;
  end;

  IOTAEditorServices70 = interface(IOTAEditorServices60)
    ['{2596F557-44A3-49A6-867E-91E21E00F53E}']
    { GetEditOptions returns the IOTAEditOptions associated with the
      IDString passed in, or nil if none was found. }
    function GetEditOptions(const IDString: string): IOTAEditOptions;
    { GetEditOptionsForFile returns the IOTAEditOptions for a particular
      FileName, defaulting to the default options. }
    function GetEditOptionsForFile(const FileName: string): IOTAEditOptions;
    { AddEditOptions returns the current IOTAEditOptions with IDString,
      or adds a new one if none was found. }
    function AddEditOptions(const IDString: string): IOTAEditOptions;
    { DeleteEditOptions deletes the IOTAEditOptions with IDString,
      raising an exception if IDString was not found. }
    procedure DeleteEditOptions(const IDString: string);
    { GetEditOptionsCount/EditOptionsCount are the count of the IOTAEditOptions }
    function GetEditOptionsCount: Integer;
    { GetEditOptionsIndex is for iterating through the IOTAEditOptions }
    function GetEditOptionsIndex(Index: Integer): IOTAEditOptions;

    property EditOptionsCount: Integer read GetEditOptionsCount;
    property EditorOptions[Index: Integer]: IOTAEditOptions read GetEditOptionsIndex;
  end;

  IOTAEditorServices80 = interface(IOTAEditorServices70)
    ['{F37E8C46-0A02-4FD4-8D57-E55F9A5783EC}']
    function AddNotifier(const Notifier: INTAEditServicesNotifier): Integer;
    procedure RemoveNotifier(Index: Integer);
  end;

  IOTAEditorServices = interface(IOTAEditorServices80)
    ['{BE733055-5ED8-45B4-BAB1-19C46C237408}']
    function GetEditOptionsIDString(const FileName: String): string;
  end;

  INTAEditorServices = interface(IUnknown)
    ['{3CC6849A-6C72-49F8-BF63-E95083789141}']
    function GetEditWindowCount: Integer;
    function GetEditWindow(Index: Integer): INTAEditWindow;
    function GetTopEditWindow: INTAEditWindow;

    property EditWindowCount: Integer read GetEditWindowCount;
    property EditWindow[Index: Integer]: INTAEditWindow read GetEditWindow;
    property TopEditWindow: INTAEditWindow read GetTopEditWindow;
  end;

  { Implement INTACustomEditorView to add a new tab along the top of the editor view }
  { If an INTACustomEditorView also implements IOTACustomEditorViewStructure,
    then the IOTACustomEditorViewStructure will be queried for the context to
    use to populate the Structure view.  See StructureViewAPI.pas }
  INTACustomEditorView = interface(IInterface)
    ['{E9465FAF-B671-4098-9ED9-AE4C05C5454A}']
    { GetCanCloneView indicates whether this view type can be open in more than one
      editor window.  This is used when the user opens a new editor window via the
      View | New Edit Window menu in the IDE.  If this view is shown as part of the
      current module, and GetCanCloneView returns False, then the module will be moved
      to the new editor window.  If GetCanCloneView returns True, then the module
      will be copied into the new editor window, making is so that it is open in two
      places (the existing editor window and the new editor window }
    function GetCanCloneView: Boolean;
    { If GetCanCloneView returns True, then CloneEditorView will be called when
      the view needs to be cloned (for instance, to be shown in another editor
      window) }
    function CloneEditorView: INTACustomEditorView;
    { Returns the string to be shown in the editor view }
    function GetCaption: string;
    { Returns a string to show in the Editor Window Caption when this
      view is active.  This is only shown with a floating Editor Window (in either
      an un-docked layout or when using View | New Edit Window }
    function GetEditorWindowCaption: string;

    { The ViewIdentifier MUST be a unique ID for this view. It is internal,
      and should never change.  It should not be translated }
    function GetViewIdentifier: string;

    { Allows the view to control the enabled state of the clipboard commands on
      the IDE's "Edit" menu when this view is active }
    function GetEditState: TEditState;
    { Called when the user uses one of the clipboard commands on the IDE's "Edit"
      menu }
    function EditAction(Action: TEditAction): Boolean;
    { Called when the user selects "File | Close All".  Set ShouldClose to False
      to prevent your view from closing in response to a Close All.  Note: this
      should be done in very special cases only (for instance, the IDE's Welcome
      Page stays open when Close All is selected }
    procedure CloseAllCalled(var ShouldClose: Boolean);

    { Called when the view is selected }
    procedure SelectView;
    { Called when the view is de-selected }
    procedure DeselectView;

    { Returns the frame class for this editor view or editor sub-view }
    function GetFrameClass: TCustomFrameClass;
    { Called when an instance of the specified frame class is created }
    procedure FrameCreated(AFrame: TCustomFrame);

    property CanCloneView: Boolean read GetCanCloneView;
    property Caption: string read GetCaption;
    property EditorWindowCaption: string read GetEditorWindowCaption;
    property FrameClass: TCustomFrameClass read GetFrameClass;
    property ViewIdentifier: string read GetViewIdentifier;
  end;
  {$IFDEF LINUX}
  {$NODEFINE INTACustomEditorView}
  {$ENDIF}

  { If you want to have view-specific state information for this Editor View
    saved in the desktop file, you should also implement INTACustomEditorViewState }
  INTACustomEditorViewState = interface(IInterface)
    ['{AA6AA3D4-1A63-4A93-8964-450484F4B4D8}']
    { LoadViewState and SaveViewState are called when a desktop is loaded and saved.
      You should use the provided TCustomIniFile and the ViewDeskSection to read and
      write state information for this view to the desktop file }
    procedure LoadViewState(const Desktop: TCustomIniFile; const ViewDeskSection: string);
    procedure SaveViewState(const Desktop: TCustomIniFile; const IsProject: Boolean;
      const ViewDeskSection: string);
  end;
  {$IFDEF LINUX}
  {$NODEFINE INTACustomEditorViewState}
  {$ENDIF}

  { If you want view-specific information disaplyed in the editor window's status
    bar, you should also implement INTACustomEditorViewStatusPanel }
  INTACustomEditorViewStatusPanel = interface(IInterface)
    ['{39B8ECEB-3048-4733-BF3D-C76540D87AE2}']
    { Return the number of panels to include in the status bar }
    function GetStatusPanelCount: Integer;
    { Called to configure the status bar panels for this editor view.  Called for
      each Panel (check "Panel.Index" to see which TStatusPanel is being configured).
      Set properties on "Panel" to configure the panel.  Set "Panel.Style" to
      "psOwnerDraw" to have DrawPanel procedure called when painting the panel }
    procedure ConfigurePanel(StatusBar: TStatusbar; Panel: TStatusPanel);
    { Called when a "psOwnerDraw" status bar panel needs to be painted }
    procedure DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
  end;
  {$IFDEF LINUX}
  {$NODEFINE INTACustomEditorViewStatusPanel}
  {$ENDIF}

  { Implement INTACustomEditorSubView to add a new lower tab in the editor }
  { For methods that take "AContext: IInterface" parameter, you can use one of
    the "ContextTo..." methods in INTAEditorViewServices to convert a non-nil
    AContext to something meaningful }
  { If an INTACustomEditorSubView also implements IOTACustomEditorSubViewStructure,
    then the IOTACustomEditorSubViewStructure will be queried for the context to
    use to populate the Structure view.  See StructureViewAPI.pas }
  INTACustomEditorSubView = interface(IInterface)
    ['{655AA26C-5898-4DB5-B21F-4F55E9B2B407}']
    { GetCanCloneView indicates whether this view type can be open in more than one
      editor window.  This is used when the user opens a new editor window via the
      View | New Edit Window menu in the IDE.  If this view is shown as part of the
      current module, and GetCanCloneView returns False, then the module will be moved
      to the new editor window.  If GetCanCloneView returns True, then the module
      will be copied into the new editor window, making is so that it is open in two
      places (the existing editor window and the new editor window }
    function GetCanCloneView: Boolean;
    { Returns the string to be shown in the editor tab }
    function GetCaption: string;
    { Indicates the location of the tab for this editor view.  The "Code" tab will
      always be the left-most tab.  All other tabs are shown in priority order.
      The form designer is shown at "HighViewPriority" }
    function GetPriority: Integer;
    { The ViewIdentifier MUST be a unique ID for this view. It is internal,
      and should never change.  It should not be translated }
    function GetViewIdentifier: string;
    { Called when the view is displayed or hidden ("hidden" is indicated by a
      nil "AContext"). AViewObject is the frame instance created for this view }
    procedure Display(const AContext: IInterface; AViewObject: TObject);
    { Called when the user uses one of the clipboard commands on the IDE's "Edit"
      menu }
    function EditAction(const AContext: IInterface; Action: TEditAction; AViewObject: TObject): Boolean;
    { Allows the view to control the enabled state of the clipboard commands on
      the IDE's "Edit" menu when this view is active }
    function GetEditState(const AContext: IInterface; AViewObject: TObject): TEditState;
    { Indicates whether or not this view handles the specified Context }
    function Handles(const AContext: IInterface): Boolean;
    { Hide happens before this INTACustomEditorSubView is hidden and another one
      is shown.  It will actually be hidden with a call to Display(nil, AViewObject) }
    procedure Hide(const AContext: IInterface; AViewObject: TObject);
    { This function is called when the editor window is closed/destroyed.  This
      is an indication that this view object should forget any information
      associated with this window/view.

      NOTE: It does not happen on a per module basis, but rather when the
      ENTIRE edit window is closed. The current open View items will then get
      this notification. Use Destroyed on the module to find out when
      a particular module is closed }
    procedure ViewClosed(const AContext: IInterface; AViewObject: TObject);

    { Returns the frame class for this editor view or editor sub-view}
    function GetFrameClass: TCustomFrameClass;
    { Called when an instance of the specified frame class is created }
    procedure FrameCreated(AFrame: TCustomFrame);

    property CanCloneView: Boolean read GetCanCloneView;
    property Caption: string read GetCaption;
    property FrameClass: TCustomFrameClass read GetFrameClass;
    property Priority: Integer read GetPriority;
    property ViewIdentifier: string read GetViewIdentifier;
  end;
  {$IFDEF LINUX}
  {$NODEFINE INTACustomEditorSubView}
  {$ENDIF}

  TNTARecreateEditorViewFunction = function: INTACustomEditorView;

  IOTAEditorViewServices = interface(IInterface)
    ['{CCE8FBE0-E121-450C-9366-1AC6BFD6CF81}']
    { Registers a custom editor view.  The RecreateEditorView function will be called
      when a saved desktop that is being loaded contains an instance of this editor
      view }
    procedure RegisterEditorView(const ViewIdentifier: string; RecreateEditorView: TNTARecreateEditorViewFunction);
    { Unregisters a previously registered custom editor view }
    procedure UnregisterEditorView(const ViewIdentifier: string);
    { Show the specified editor tab in the top most editor window }
    procedure ShowEditorView(const EditorView: INTACustomEditorView);
    { Returns the INTAEditWindow that owns the specified EditorTab }
    function GetOwningEditWindow(const EditorTab: INTACustomEditorView): INTAEditWindow;

    function RegisterEditorSubView(const ACustomEditorView: INTACustomEditorSubView): Pointer;
    procedure UnregisterEditorSubView(AView: Pointer);
    { The following are helper functions to convert an Editor Context to something more useful }
    function ContextToRootComponent(const AContext: IInterface; out ARootComponent: TComponent): Boolean;
    function ContextToDesigner(const AContext: IInterface; out ADesigner: IDesigner): Boolean;
    function ContextToFormEditor(const AContext: IInterface; out AFormEditor: IOTAFormEditor): Boolean;
    function ContextToModule(const AContext: IInterface; out AModule: IOTAModule): Boolean;
    function ContextToFormFilename(const AContext: IInterface; out AFilename: string): Boolean;
    function ContextToFilename(const AContext: IInterface; out AFilename: string): Boolean;
    function ContextResurrect(const AContext: IInterface): Boolean;
  end;

  INTAToDoItem = interface(IUnknown)
    ['{094003D8-E7AA-11D2-AA99-00C04FA35CE8}']
    { Implement this interface on items returned by a registered IOTAToDoManager.
      This interface is also returned by IOTAToDoServices.GetItem to allow read
      access to the IDE's To-Do List. }

    { CanDelete returns true to enable "Delete" on the To-Do List's local menu when this item
      is selected. }
    function  CanDelete: Boolean;
    { CanEdit returns true to enable "Edit" on the To-Do List's local menu when this item
      is selected. }
    function  CanEdit: Boolean;
    { CanShow returns true to enable "Open" on the To-Do List's local menu when this item
      is selected. }
    function  CanShow: Boolean;
    { Delete is called when the item has been selected for deletion by the user. }
    procedure Delete;
    { DrawImage draws an image representing the item in the To-Do List window. }
    procedure DrawImage(const Canvas: TCanvas; const Rect: TRect);
    { DoubleClicked indicates the user has double-clicked the item in the To-Do List window. }
    procedure DoubleClicked;
    procedure Edit;
    function  GetText: string;
    function  GetPriority: TOTAToDoPriority;
    function  GetCategory: string;
    function  GetChecked: Boolean;
    { GetModuleName returns the string shown in the "Module" column of the To-Do List
      window. }
    function  GetModuleName: string;
    { GetKind returns a unique string indicating the origin of the item. It is also used
      as the caption for submenu items of the "Filter" local menu. }
    function  GetKind: string;
    function  GetData: Integer;
    function  GetOwner: string;
    { IsValid indicates when the item has been completely defined (when processing
      items in a separate thread). }
    function  IsValid: Boolean;
    procedure SetChecked(const Value: Boolean);
    { Show is called to make the item's source visible; i.e., for an item culled
      from source code, Show opens the source file in the editor. }
    procedure Show;

    property  Checked: Boolean read GetChecked write SetChecked;
  end;
  {$IFDEF LINUX}
  {$NODEFINE INTAToDoItem}
  {$ENDIF}

  IOTAToDoManager = interface(IUnknown)
    ['{3D4A0565-EB77-11D2-AA9A-00C04FA35CE8}']
    { Implement IOTAToDoManager on a TInterfacedObject; register with IOTAToDoServices
      to add To-Do items to the list. The Manager is basically just a list of
      INTAToDoItem interfaces; INTAToDoItem methods will be called when the To-Do List
      needs to display information, paint the item's representation in the list,
      inquire about status of an item. Not all INTAToDoItems may be added to the IDE's
      list; if the user has requested not to see items with a given attribute
      (i.e., Checked or of a certain type), they will not be added to the displayed
      To-Do list. }

    { The Name property is used to uniquely identify To-Do Item managers. }
    function  GetName: string;
    procedure SetName(const AName: string);
    function  GetItem(Index: Integer): INTAToDoItem;
    function  GetItemCount: Integer;
    { ProjectChanged is called when the active project has been changed. }
    procedure ProjectChanged;
    property  Name: string read GetName write SetName;
    property  ItemCount: Integer read GetItemCount;
    property  Items[Index: Integer]: INTAToDoItem read GetItem;
  end;

  IOTAToDoServices = interface(IUnknown)
    ['{F8FC00EF-E61A-11D2-AA99-00C04FA35CE8}']
    function  AddManager(AManager: IOTAToDoManager): Integer;
    function  AddNotifier(const ANotifier: IOTANotifier): Integer;
    { GetItem returns an item in the To-Do list, including those belonging to add-in
      managers. }
    function  GetItem(Index: Integer): INTAToDoItem;
    { Returns the total number of visible To-Do items in the To-Do List, including
      those belonging to add-in managers. }
    function  GetItemCount: Integer;
    procedure RemoveManager(Index: Integer);
    procedure RemoveNotifier(Index: Integer);
    { UpdateList causes the To-Do List to collate all items provided by its managers.
      Note that it will force the To-Do List to call every registered manager to
      query the state of all its items. }
    procedure UpdateList;
    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: INTAToDoItem read GetItem;
  end;

  { types and interfaces for code completion }
  TOTAInvokeType = (itAuto, itManual, itTimer);

  TOTACodeInsightType = (citNone, { don't invoke code insight }
                         citCodeInsight, { perform code completion }
                         citParameterCodeInsight, { perform parameter completion }
                         citBrowseCodeInsight, { browse code (CTRL + click) }
                         citHintCodeInsight); { pop up hints over idents in the editor }

  TOTASortOrder = (soAlpha, { sort the code completion list alphanumerically }
                   soScope); { sort the code completion list by scope }

  { TOTAViewerSymbolFlags is used for color coding the class name of the ident }
  TOTAViewerSymbolFlags = (vsfUnknown,
                           vsfConstant,
                           vsfType,
                           vsfVariable,
                           vsfProcedure,
                           vsfFunction,
                           vsfUnit,
                           vsfLabel,
                           vsfProperty,
                           vsfConstructor,
                           vsfDestructor,
                           vsfInterface,
                           vsfEvent,
                           vsfParameter,
                           vsfLocalVar);
  { TOTAViewerVisibilityFlags }
  TOTAViewerVisibilityFlags = Integer;

  { TOTAProcDispatchFlags for future use }
  TOTAProcDispatchFlags = (pdfNone,
                           pdfVirtual,
                           pdfDynamic);

  IOTACodeInsightSymbolList = interface(IUnknown)
    ['{4CA1CDFD-BD9A-4628-94AE-9BF3EB2DA22E}']
    { Implementor should clear its symbol list }
    procedure Clear;
    { returns the count of the symbols in the list - may be modified by setting a filter - }
    function GetCount: Integer;
    { returns whether the symbol is able to be read from and written to }
    function GetSymbolIsReadWrite(I: Integer): Boolean;
    { returns whether the symbols is abstract.  Viewer draws these in the 'need to implement' color }
    function GetSymbolIsAbstract(I: Integer): Boolean;
    { return the symbol flags for the item at index 'I'.  I is the index in the filtered list }
    function GetViewerSymbolFlags(I: Integer): TOTAViewerSymbolFlags;
    { return the visibility flags for the item at index 'I'.  I is the index in the filtered list }
    function GetViewerVisibilityFlags(I: Integer): TOTAViewerVisibilityFlags;
    { return the procedure flags for the item at index 'I'.  I is the index in the filtered list }
    function GetProcDispatchFlags(I: Integer): TOTAProcDispatchFlags;
    { The list was requested to be sorted by 'Value' }
    procedure SetSortOrder(const Value: TOTASortOrder);
    { returns the sort order of the list }
    function GetSortOrder: TOTASortOrder;
    { given an identifier, return the index of the closest partial match }
    function FindIdent(const AnIdent: string): Integer;
    { given an identifier, find the 'Index' of an exact match in the list and return True.  Otherwise return False }
    function FindSymIndex(const Ident: string; var Index: Integer): Boolean;
    { set the lists filter to 'FilterText'.  It is up to the implementor to determine how to filter or if they even want to filter }
    procedure SetFilter(const FilterText: string);
    { return the symbol text for item 'Index'.  i.e. Form1 }
    function GetSymbolText(Index: Integer): string;
    { return the symbol type text for item 'Index'.  i.e. TForm1 }
    function GetSymbolTypeText(Index: Integer): string;
    { return the symbol class text for item 'Index'.  i.e. 'var', 'function', 'type', etc }
    function GetSymbolClassText(I: Integer): string;
    property SymbolClassText[I: Integer]: string read GetSymbolClassText;
    property SymbolTypeText[I: Integer]: string read GetSymbolTypeText;
    property SymbolText[I: Integer]: string read GetSymbolText;
    property SymbolFlags[I: Integer]: TOTAViewerSymbolFlags read GetViewerSymbolFlags;
    property SymbolVisibility[I: Integer]: TOTAViewerVisibilityFlags read GetViewerVisibilityFlags;
    property SymbolIsAbstract[I: Integer]: Boolean read GetSymbolIsAbstract;
    property SymbolIsReadWrite[I: Integer]: Boolean read GetSymbolIsReadWrite;
    property FuncDispatchFlags[I: Integer]: TOTAProcDispatchFlags read GetProcDispatchFlags;
    property SortOrder: TOTASortOrder read GetSortOrder write SetSortOrder;
    property Count: Integer read GetCount;
  end;

  IOTACodeInsightSymbolList80 = interface(IOTACodeInsightSymbolList)
    { Return documentation for the symbol, in HTML } 
    function GetSymbolDocumentation(I: Integer): string;
    property SymbolDocumentation[I: Integer]: string read GetSymbolDocumentation;
  end;

  IOTACodeInsightParamQuery = interface(IUnknown)
    ['{B1842926-C7F7-4869-B55A-CFDB6BF705B5}']
    { returns the count of the parameters }
    function GetQueryParamCount: Integer;
    { returns a string representing the return value of the method/proc }
    function GetQueryRetVal: string;
    { returns the symbol text of the parameter at 'Index' }
    function GetQueryParamSymText(Index: Integer): string;
    { returns the symbol type text of the parameter at 'Index' }
    function GetQueryParamTypeText(Index: Integer): string;
    { returns whether the symbol at index 'Index' as a default value }
    function GetQueryParamHasDefaultVal(Index: Integer): Boolean;
    { returns a string representing the type of invocation for the parameter at 'Index'. i.e. 'var', 'out', 'const' }
    function GetQueryParamInvokeTypeText(Index: Integer): string;
    property RetValType: string read GetQueryRetVal;
    property ParamSymText[Index: Integer]: string read GetQueryParamSymText;
    property ParamTypeText[Index: Integer]: string read GetQueryParamTypeText;
    property ParamCount: Integer read GetQueryParamCount;
    property ParamHasDefaultVal[Index: Integer]: Boolean read GetQueryParamHasDefaultVal;
    property ParamInvokeTypeText[Index: Integer]: string read GetQueryParamInvokeTypeText;
  end;

  IOTACodeInsightParameterList = interface(IUnknown)
    ['{99B6A644-3E97-48A1-9758-0A5FE94767C7}']
    {
      returns a ParamQuery to the caller based upon the ProcIndex.  There may be multiple
      items in list as we may be dealing with overloaded functions.
    }
    procedure GetParameterQuery(ProcIndex: Integer; out ParamQuery: IOTACodeInsightParamQuery);
    {
      return which character to use to delimit parameters in the parameter hint window.
      i.e.  Delphi uses ';'
            C++ uses ','
    }
    function GetParamDelimiter: Char;
    { returns the count of procedures in the list }
    function GetProcedureCount: Integer;
    {
      returns the parameters as a string from the procedure at index I.  The parameters
      should be delimited by a line ending (sLineBreak for instance).
    }
    function GetProcedureParamsText(I: Integer): string;
    property ProcedureParamsText[I: Integer]: string read GetProcedureParamsText;
    property ProcedureCount: Integer read GetProcedureCount;
    property ParamDelimiter: Char read GetParamDelimiter;
  end;

  IOTACodeInsightParameterList100 = interface(IOTACodeInsightParameterList)
    ['{EC7B37F6-8AB8-4B09-85A4-AA53D5856C0F}']
    function GetParmPos(Index: Integer): TOTACharPos;
    function GetParmCount: Integer;
    function GetParmName(Index: Integer): string;
    function GetParmHint(Index: Integer): string;
    function GetCallStartPos: TOTACharPos;
    function GetCallEndPos: TOTACharPos;

    property CallStartPos: TOTACharPos read GetCallStartPos;
    property CallEndPos: TOTACharPos read GetCallEndPos;
    property ParmCount: Integer read GetParmCount;
    property ParmHint[Index: Integer]: string read GetParmHint;
    property ParmName[Index: Integer]: string read GetParmName;
    property ParmPos[Index: Integer]: TOTACharPos read GetParmPos;
  end;

  IOTACodeInsightManager100 = interface(IUnknown)
    ['{BA5B444A-6E78-4A79-BF05-E184C1132B30}']
    { returns a description of the language which we handle }
    function GetName: string;
    { returns a unique IDString to the services module }
    function GetIDString: string;
    { returns whether we should be able to be invoked or not }
    function GetEnabled: Boolean;
    { sets the active state to Value so this manager may be turned off }
    procedure SetEnabled(Value: Boolean);
    {
      returns a charset used to get the token at the current editor position.  This is
      used for retrieving the seed text when code completion is invoked as well as
      retrieving the token from the editor when we are typing for look ahead.
      The PreValidating parameter should be used to add special tokens to the charset for retrieval
      from the editor.  For instance, C++ might add ['.', '-', '>'] to the returned charset
      when it is prevalidating.
    }
    function EditorTokenValidChars(PreValidating: Boolean): TSysCharSet;
    {
      the implementor should set Allow to True if it wishes to be invoked for the key 'Key'.
      'Key' is the key which the user pressed to invoke code completion.
      There are four special values to 'Key' when invoked by the code insight timer.

      They are as follows:
        #0 : Code completion was requested.
        #1 : Parameter insight was requested.
        #2 : A browse was requested.
        #3 : a symbol hint was requested.
    }
    procedure AllowCodeInsight(var Allow: Boolean; const Key: Char);
    {
      the implementor should return true if it wishes to allow the token 'str' to be
      a valid code point for Code Insight.
    }
    function PreValidateCodeInsight(const Str: string): Boolean;
    { returns whether the symbol at index 'Index' as browseable in the Code completion viewer }
    function IsViewerBrowsable(Index: Integer): Boolean;
    { returns whether the code completion viewer allows multi-select }
    function GetMultiSelect: Boolean;
    { returns the symbol list to the caller }
    procedure GetSymbolList(out SymbolList: IOTACodeInsightSymbolList);
    {
      determines whether or not the key 'Key' which was entered into the editor should close
      the code completion viewer or not (set CloseViewer to True or False depending on your choice).
      Also, the implementor should inform the manager whether or not it should accept the symbol
      at the currently selected index/indices.
    }
    procedure OnEditorKey(Key: Char; var CloseViewer: Boolean; var Accept: Boolean);
    { returns true if this manager should handle this file }
    function HandlesFile(const AFileName: string): Boolean;
    { returns the longest symbol class text for measurement for the viewer.  i.e.  'constructor' is longer than 'var' }
    function GetLongestItem: string;
    { returns a parameter list to the manager }
    procedure GetParameterList(out ParameterList: IOTACodeInsightParameterList);
    {
      given key 'AChar' which was entered into the editor and the current element (atComment, atIdentifier, etc),
      return how code insight should be invoked and which type of invocation it should be.

      As an example, GetCodeInsightType() might be implemented something like this:
      ...
      begin
        InvokeType := itManual;
        if not ((AElement = atString) and (AChar <> #1)) and not (AElement = atComment) then
        begin
          case AChar of
            #0: CodeInsightType := citCodeInsight;
            #1: CodeInsightType := citParameterCodeInsight;
            #2: CodeInsightType := citBrowseCodeInsight;
            #3: CodeInsightType := citHintCodeInsight;
            '.':
            begin
              CodeInsightType := citCodeInsight;
              InvokeType := itTimer;
            end;
            '(':
            begin
              CodeInsightType := citParameterCodeInsight;
              InvokeType := itTimer;
            end;
          end;
        end
        else
          CodeInsightType := citNone;
      end;
    }
    procedure GetCodeInsightType(AChar: Char; AElement: Integer; out CodeInsightType: TOTACodeInsightType;
      out InvokeType: TOTAInvokeType);
    {
      returns true if invocation was successful.  HowInvoked informs the implementor whether
      it was invoked via timer, manual, etc...  Str is the text to seed to viewer with and
      is used for the initial filtering in the viewer.
    }
    function InvokeCodeCompletion(HowInvoked: TOTAInvokeType; var Str: string): Boolean;
    {
      returns true if invocation was successful.  HowInvoked informs the implementor whether
      it was invoked via timer, manual, etc...  SelectedIndex is the index of the current parameter
      for the method/proc.
    }
    function InvokeParameterCodeInsight(HowInvoked: TOTAInvokeType; var SelectedIndex: Integer): Boolean;
    {
      tells the manager where it should anchor the parameter hint window.
      A default value (EdPos) is provided for the implementor to change if they so wish.
    }
    procedure ParameterCodeInsightAnchorPos(var EdPos: TOTAEditPos);
    {
      returns the index of the parameter which should be highlighted based upon EdPos.
      This is used to reduce extra codeinsight invocations as an implementor might
      store off the editor positions of parameters on the first invocation.
      return a -1 if you want to be reinvoked.
    }
    function ParameterCodeInsightParamIndex(EdPos: TOTAEditPos): Integer;
    { return the hint string for the position in the editor (HintLine/HintCol are the editor coordinates) }
    function GetHintText(HintLine, HintCol: Integer): string;
    {
      return a FileName and LineNumber for the symbol which is requested to be browsed to.
      if Index > -1 then it is an index into the symbol list and the browse was requested
      by a user clicking in the code completion viewer.
      return false if you'd like to inform the user that the requested operation failed otherwise return true.
      if you wish to fail by not informing the user, set AFileName = '' and ALineNum = 0.
      if Index is -1, you should use the global CodeInsightServices() and request the EditView from it.
      This should be able to give you any information you require.
    }
    function GotoDefinition(out AFileName: string; out ALineNum: Integer; Index: Integer = -1): Boolean;
    {
      called when the code completion is completed.  Accepted is true if the user has requested
      the item hinted to them in the viewer otherwise Accepted is false.
      DisplayParams should be set to true if the implementor would like to be requeried
      for parameter invocation.  It is up to the implementor to insert the text into the editor.
      One way might be to use CodeInsightServices.InsertText(StrToInsert, ShouldReplace);
      Another might be to acquire the EditView from CodeInsightServices.GetEditView() and do
      the insertion yourself.
    }
    procedure Done(Accepted: Boolean; out DisplayParams: Boolean);
    property Name: string read GetName;
    property MultiSelect: Boolean read GetMultiSelect;
    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;

  /// <summary>
  /// Used for the new Code Insight Preview feature. To be implemented by a module,
  /// and queried for by Code Insight. Duplicated in ToolsAPI.cs. If you change it
  /// here, you MUST change it in ToolsAPI.cs
  /// </summary>
  IOTACodeBrowsePreview = interface(IDispatch)
    ['{929C8812-4DB1-4338-B3B8-C1BE7969E2BF}']
    function GetCodePreviewInfo(SourceLine: Integer; SourceCol: Integer;
      out FileName: WideString; out Offset: Integer; out Length: Integer): WordBool; safecall;
  end;

  IOTACodeInsightManager90 = interface(IOTACodeInsightManager100)
    ['{3408E3C3-B6C1-4B02-8C5C-FD54D71A082F}']
    /// <summary>
    /// Retrieves help insight information for the current Viewer's
    /// selected item. Return an empty string if none is to be shown or
    /// is available.
    /// </summary>
    function GetHelpInsightHtml: WideString;
  end;

  IOTACodeInsightManager = interface(IOTACodeInsightManager100)
    ['{013F5F71-C500-44C8-904C-554F98C2EC28}']
    function GetOptionSetName: string;
  end;

  /// <summary>
  ///
  /// TOTACodeCompletionContext
  ///
  /// Primary code completion managers use this to let non-primary managers
  /// filter their data according to the invocation context.
  /// </summary>
  TOTACodeCompletionContext = (ccNone,
                               ccError,
                               ccMember,
                               ccArgument,
                               ccDecl,
                               ccTypeDecl,
                               ccExpr,
                               ccStatement,
                               ccConstExpr,
                               ccProcDecl,
                               ccMemberDecl,
                               ccNamespace,
                               ccComment,
                               ccStringLiteral,
                               // top level
                               ccDocument,
                               // in an element
                               ccElement,
                               // in an attribute
                               ccAttribute,
                               // all contexts
                               ccAny);

  /// <summary>
  ///
  /// IOTAPrimaryCodeInsightManager
  ///
  /// Tells code insight services that the manager which implements this
  /// is a primary manager.  This means that alternate managers
  /// may query the 'currentmanager' from the services to get
  /// invocation context information.
  /// </summary>
  IOTAPrimaryCodeInsightManager = interface(IDispatch)
    ['{E935146E-B88C-4E4F-9FAC-69973952D534}']
    function GetContext: TOTACodeCompletionContext;
    property Context: TOTACodeCompletionContext read GetContext;
  end;

  IOTACodeInsightViewer90 = interface(IUnknown)
    ['{AAA55FAC-350E-4F43-9C42-4FC28B6BFE33}']
    { returns whether the item at index 'Index' is selected in the viewer } 
    function GetSelected(Index: Integer): Boolean;
    { returns the count of the items in the viewer }
    function GetItemCount: Integer;
    { returns the selected string.  Is useful when implementing IOTACodeInsightManager.Done() }
    function GetSelectedString: string;
    { returns the index of the selected item }
    function GetSelectedIndex: Integer; deprecated;
    {
      returns the key which was used to close the viewer.  An implementation
      of IOTACodeInsightManager.Done() might use this to determine whether it should be inserted
      into the completed text or not.  For instance, a user might accept an identifier in the
      list by typing a ';'.  The comma would be the 'closekey' and the implementor of
      IOTACodeInsightManager.Done() might want to insert the identifier selected as well
      as the ';'.
    }
    function GetCloseKey: Char;
    { returns whether the selection in the viewer is valid }
    function GetIsValidSelection: Boolean; deprecated;
    property Selected[Index: Integer]: Boolean read GetSelected;
    property ItemCount: Integer read GetItemCount;
    property SelectedString: string read GetSelectedString;
    property SelectedIndex: Integer read GetSelectedIndex;
    property CloseKey: Char read GetCloseKey;
    property IsValidSelection: Boolean read GetIsValidSelection;
  end;

  IOTACodeInsightViewer = interface(IOTACodeInsightViewer90)
    ['{6268E073-D469-41BB-84C5-D96A34EA3D17}']
    function GetManagerIsValidSelection(const Mgr: IOTACodeInsightManager): Boolean; overload;
    function GetManagerIsValidSelection(const Mgr: IOTACodeInsightManager; Index: Integer): Boolean; overload;
    function GetManagerSelectedIndex(const Mgr: IOTACodeInsightManager): Integer;
  end;

  {
    An IOTACodeInsightManager should implement this interface if it would like to
    do custom drawing in the viewer.
  }
  INTACustomDrawCodeInsightViewer = interface(IUnknown)
    ['{32CA7B43-9AFC-49CF-ABC9-7ECD772488D9}']
    {
      called when the viewer draws the item at index 'Index'.  if DoDraw is false,
      then only a rectangle calculation is being requested.  The rectangle should
      be returned by the 'Rect' out parameter.
    }
    procedure DrawLine(Index: Integer; Canvas: TCanvas; var Rect: TRect;
      DrawingHintText: Boolean; DoDraw: Boolean; var DefaultDraw: Boolean);
  end;
  {$IFDEF LINUX}
  {$NODEFINE INTACustomDrawCodeInsightViewer}
  {$ENDIF}

  { may be queried by using (BorlandIDEServices as IOTACodeInsightServices). }
  IOTACodeInsightServices60 = interface(IUnknown)
    ['{476904F8-89A9-4CD8-A71E-164660659763}']
    { returns the current edit view to work with }
    procedure GetEditView(out EditView: IOTAEditView);
    { returns the viewer interface to be queried by code completion implementors }
    procedure GetViewer(out Viewer: IOTACodeInsightViewer);
    { returns the current code completion manager }
    procedure GetCurrentCodeInsightManager(out CodeInsightManager: IOTACodeInsightManager);
    { cancel the invocation requested from the current IOTACodeInsightManager }
    procedure CancelCodeInsightProcessing;
    { registers a new code insight manager and returns its index }
    function AddCodeInsightManager(const ACodeInsightManager: IOTACodeInsightManager): Integer;
    { unregister the manager at index 'Index' }
    procedure RemoveCodeInsightManager(Index: Integer);
    { inserts 'Str' into the editor optionally replacing the token which the editor is currently on }
    procedure InsertText(const Str: string; Replace: Boolean);
    { returns the count of installed code insight managers }
    function GetCodeInsightManagerCount: Integer;
    { returns the code insight manager at index 'Index' }
    function GetCodeInsightManager(Index: Integer): IOTACodeInsightManager;
    property CodeInsightManagerCount: Integer read GetCodeInsightManagerCount;
    property CodeInsightManager[Index: Integer]: IOTACodeInsightManager read GetCodeInsightManager;
  end;

  IOTACodeInsightServices = interface(IOTACodeInsightServices60)
    ['{EE5C42A9-DBC8-4C5D-B28E-528024CBC97C}']
    {
      sets the EditView and CurrentCIManager for operations with CI.  The
      Viewer/Hints are disabled while an EditView and/or a CodeInsightManager
      have been set via this procedure.  SetQueryContext must be called with
      parameters of nil for CodeInsightServices to be returned to its initial state.
    }
    procedure SetQueryContext(const EditView: IOTAEditView;
      const CodeInsightManager: IOTACodeInsightManager);
  end;

  { Represents a searchable item in the IDE Insight dialog }
  INTAIDEInsightItem = interface
    ['{539EE5EB-7A17-4756-9E98-09EE71551AE0}']
    { Provides a canvas on which to custom draw the item's text and specify its
      width.
      Return value: Width of the text that is to be drawn. If this value is not
        accurate clipping may occur.
      Canvas: The canvas on which to draw.
      Rect: The visible area in which to draw. The return value of this function
        may be larger than Rect's width indicating a horizontal scrollbar is necessary.
      DrawDefault: Indicates whether the item is to be drawn in the default manner
        by the IDE or is custom drawn.
      DoDraw: If false, the return value is being requested for measurement purposes
        only, to determine hint or scrollbar parameters.
    }
    function DrawText(Canvas: TCanvas; Rect: TRect; var DrawDefault: Boolean;
      DoDraw: Boolean = True): Integer;
    { Called when this item has been selected and the dialog accepted }
    procedure Execute;
    { Return a description of the item. Default drawing de-emphasises this
      text, since by default it is not searchable }
    function GetDescription: string;
    { Returns whether or not the item's description should be included when
      determining a match }
    function GetDescriptionSearchable: Boolean;
    { Provide a 16x16 pixel custom glyph. This TBitmap is owned by the IDE and
      should be drawn on but not be assigned to. Return false if no glyph is
      to be drawn. }
    function GetGlyph(Bitmap: TBitmap): Boolean;
    { Determines ownership and lifetime of this item. If true, the IDE assumes
      ownership of the item and it will persist throughout the current IDE
      session, and should only to be added to the list once. If false, the
      item must be added to the list on each request and will be released
      when the dialog is closed. }
    function GetSticky: Boolean;
    { The searchable title of the item }
    function GetTitle: string;
    { Whether or not the item is visible (searchable) in the current IDE context. }
    function GetVisible: Boolean;
    { Called just before the IDE insight dialog is populated. Update the item's
      visibility state and other parameters if necessary. }
    procedure Update;

    property Description: string read GetDescription;
    property IsDescriptionSearchable: Boolean read GetDescriptionSearchable;
    property Sticky: Boolean read GetSticky;
    property Title: string read GetTitle;
    property Visible: Boolean read GetVisible;
  end;
  INTAIDEInsightItemArray = array of INTAIDEInsightItem;

  { Represents a category in the IDE Insight dialog }
  IOTAIDEInsightCategory = interface
    ['{D943D3FE-BC3F-4405-BF2A-4F9F001AD19E}']
    { Removes all items from the category regardless of persistent (Sticky) state }
    procedure ClearItems;
    { Return the display name of the category, e.g., "Commands", "Files", etc. }
    function GetCaption: string;
    { Whether or not the category is enabled for searching and display }
    function GetDisabled: Boolean;
    { Returns an item in this category }
    function GetItem(const Index: Integer): INTAIDEInsightItem;
    { The number of items in the category }
    function ItemCount: Integer;
    { Disables the category. Disabled categories will not show in the dialog
      and will not be searched for matching items. }
    procedure SetDisabled(Value: Boolean);

    property Caption: string read GetCaption;
    property Disabled: Boolean read GetDisabled write SetDisabled;
    property Items[const Index: Integer]: INTAIDEInsightItem read GetItem;
  end;

  IOTAIDEInsightService = interface;
  { Implement and register this interface with IOTAIDEInsightService to receive
    a notification when the IDE Insight dialog is invoked.
    e.g., (BorlandIDEServices as IOTAIDEInsightService).AddNotifier() }
  IOTAIDEInsightNotifier = interface
    ['{FDEC7D0D-9633-424A-A925-5DC1DE13FD48}']
    { This notification is called when the IDE Insight dialog is being invoked and
      is requesting items for its list. Context parameter is null and reserved
      for future use. }
    procedure RequestingItems(IDEInsightService: IOTAIDEInsightService; Context: IInterface);
  end;

  { Entry point into the IDE Insight system. }
  IOTAIDEInsightService = interface
    ['{D1258D2C-DE95-4A0A-9A7E-8C6167F48B31}']
    { Adds an item into the specified category. If the category does not exist
      it will be created. If category is empty, the item will appear outside
      of the category list }
    procedure AddItem(Item: INTAIDEInsightItem; Category: string = '');
    { Call this to register an IOTAIDEInsightNotifier. The result is the index to be
      used when calling RemoveNotifier. If <0 then an error occurred. }
    function AddNotifier(Notifier: IOTAIDEInsightNotifier): Integer;
    { The number of categories currently known by IDE Insight }
    function CategoryCount: Integer;
    { Returns a category by display name or index }
    function GetCategory(const IndexOrName: Variant): IOTAIDEInsightCategory; overload;
    { Returns a category by name and optionally creates it }
    function GetCategory(const Name: string; CanCreate: Boolean = False): IOTAIDEInsightCategory; overload;
    { Invokes the IDE Insight dialog }
    procedure Invoke;
    { Remove an Item from the list if it doesn't have a category }
    procedure RemoveItem(const Item: INTAIDEInsightItem);
    { Remove a previously added notifier using the index returned by AddNotifier(). }
    procedure RemoveNotifier(const Index: Integer);

    property Categories[const IndexOrName: Variant]: IOTAIDEInsightCategory read GetCategory;
  end;

  TOTAAffect = (afNothing, afTop, afLeft, afBottom, afRight, afHCenter, afVCenter,
    afHSpace, afVSpace, afHWinCenter, afVWinCenter, afHSpaceInc, afHSpaceDec,
    afHSpaceDel, afVSpaceInc, afVSpaceDec, afVSpaceDel, afAlignToGrid, afSnapToGrid,
    afSendToBack, afBringToFront);

  TOTASizeAffect = (asNothing, asHGrow, asHShrink, asHAbsolute, asVGrow, asVShrink,
    asVAbsolute, asWidths, asHeights, asWidthHeight, asSizeToGrid);

  TOTAAlignableState = set of (asEnabled, asChecked);

  IOTAAlignable = interface
    ['{346E7BA3-D47E-11D3-BA96-0080C78ADCDB}']
    procedure Align(Affect: TOTAAffect);
    procedure Size(Affect: TOTASizeAffect; Value: Integer);
  end;

  IOTAAlignableState = interface
    ['{481A90D6-95AD-4AC8-8C53-E862DF164BD7}']
    function GetAlignAffectState(Affect: TOTAAffect): TOTAAlignableState;
    function GetSizeAffectState(Affect: TOTASizeAffect): TOTAAlignableState;
  end;

  IOTAScaleable = interface
    ['{346E7BA6-D47E-11D3-BA96-0080C78ADCDB}']
    procedure Scale(Factor: Integer);
  end;

  IOTATabOrderable = interface
    ['{346E7BA4-D47E-11D3-BA96-0080C78ADCDB}']
    function GetTabCompCount: Integer;
    function GetTabCompInfo(Order: Integer; var Name, ClassName: string;
      var Comp: Pointer): Boolean;
    procedure SetTabCompOrder(Comp: Pointer; Order: Integer);
  end;

  IOTACreateOrderable = interface
    ['{346E7BA5-D47E-11D3-BA96-0080C78ADCDB}']
    function GetCompCount: Integer;
    function GetCompName(Index: Integer): string;
    function GetCompType(Index: Integer): string;
    function GetNVComp(Index: Integer): Pointer;
    procedure SetNVComp(Comp: Pointer; Order: Integer);
  end;

  { Designers must implement this interface in order to make sure they can
    participate in the Edit menu designer commands }

  IOTADesignerCommandNotifier = interface(IOTANotifier)
    ['{F862787A-4FF5-4B91-B626-D9AD53EA98A6}']
    function GetActive: Boolean;
    function GetActiveDesignerType: string;
    function GetAlignable: IOTAAlignable;
    function IsCommandEnabled(const Command: string): Boolean;
    function IsCommandChecked(const Command: string): Boolean;
    function IsCommandVisible(const Command: string): Boolean;
    procedure DesignerCommand(const Command: string);

    property Active: Boolean read GetActive;
    property ActiveDesignerType: string read GetActiveDesignerType;
    property Alignable: IOTAAlignable read GetAlignable;
  end;

  IOTADesignerCommandServices = interface(IInterface)
    ['{C18D655E-54B1-412D-BECF-584B08838827}']
    { Sets the currently activated designer to the given DesignerCommands
      interface so that the IDE can then use this interface to determine what
      stock edit menu items are enabled and where to send the responses to the
      commands }
    procedure ActivateDesignerCommands(const DesignerCommands: IOTADesignerCommandNotifier);
    { Returns the currently active set of DesignerCommands }
    function GetActiveDesignerCommands: IOTADesignerCommandNotifier;
    { Call EditAlign to use the internal IDE alignment dialog }
    procedure EditAlign(const Alignable: IOTAAlignable);
    { Call EditSize to use the internal IDE size dialog. }
    procedure EditSize(const Sizeable: IOTAAlignable);
    { Call EditScale to use the internal IDE scaling dialog }
    procedure EditScale(const Scalable: IOTAScaleable);
    { Call EditTabOrder to use the internal IDE tab order dialog }
    procedure EditTabOrder(const TabOrderable: IOTATabOrderable);
    { Call EditCreationOrder to use the internal IDE creation order dialog }
    procedure EditCreationOrder(const CreateOrderable: IOTACreateOrderable);

    property ActiveDesignerCommands: IOTADesignerCommandNotifier read GetActiveDesignerCommands;
  end;

  IOTAPersonalityServices100 = interface(IInterface)
    ['{F66FB6B3-24DC-4BC0-8A6B-4159B527A1FC}']
    function GetPersonalityCount: Integer;
    function GetPersonality(Index: Integer): string;
    function AddPersonality(const APersonality: string): Integer;
    procedure RemovePersonality(const APersonality: string);
    procedure AddPersonalityTrait(const APersonality: string; const ATraitGUID: TGUID; const ATrait: IInterface);
    procedure RemovePersonalityTrait(const APersonality: string; const ATraitGUID: TGUID);
    procedure AddFileType(const APersonality, AFileType: string);
    procedure RemoveFileType(const APersonality, AFileType: string);
    procedure AddFileExtensions(const APersonality, AFileType, AFileExtensions: string);
    procedure RemoveFileExtensions(const APersonality, AFileType, AFileExtensions: string);
    procedure AddFileTrait(const APersonality, AFileType: string; const ATraitGUID: TGUID; const ATrait: IInterface);
    procedure RemoveFileTrait(const APersonality, AFileType: string; const ATraitGUID: TGUID);
    function GetCurrentPersonality: string;
    procedure SetCurrentPersonality(const APersonality: string);
    function GetFileTrait(const APersonality, AFileName: string; const ATraitGUID: TGUID; SearchDefault: Boolean): IInterface; overload;
    function GetFileTrait(const AFileName: string; const ATraitGUID: TGUID; SearchDefault: Boolean): IInterface; overload;
    function GetFileTrait(const APersonality, AFileName: string; const ATraitGUID: TGUID): IInterface; overload;
    function GetFileTrait(const AFileName: string; const ATraitGUID: TGUID): IInterface; overload;
    function GetTrait(const APersonality: string; const ATraitGUID: TGUID): IInterface; overload;
    function GetTrait(const ATraitGUID: TGUID): IInterface; overload;
    function SupportsFileTrait(const APersonality, AFileName: string; const ATraitGUID: TGUID; SearchDefault: Boolean): Boolean; overload;
    function SupportsFileTrait(const AFileName: string; const ATraitGUID: TGUID; SearchDefault: Boolean): Boolean; overload;
    function SupportsFileTrait(const APersonality, AFileName: string; const ATraitGUID: TGUID): Boolean; overload;
    function SupportsFileTrait(const AFileName: string; const ATraitGUID: TGUID): Boolean; overload;
    function SupportsTrait(const APersonality: string; const ATraitGUID: TGUID): Boolean; overload;
    function SupportsTrait(const ATraitGUID: TGUID): Boolean; overload;
    // If there are more than one personalities that support this interface the user
    // will be asked to select one.  The select personality will be set to the active personality.
    function PromptUserForPersonality(const ATraitGUID: TGUID; const Prompt: string): Boolean;

    property CurrentPersonality: string read GetCurrentPersonality write SetCurrentPersonality;
    property PersonalityCount: Integer read GetPersonalityCount;
    property Personalities[Index: Integer]: string read GetPersonality;
  end;

  IOTAPersonalityServices = interface(IOTAPersonalityServices100)
    ['{92D70AB5-F54F-4432-8E0E-5BEEF4B3BE77}']
    function GetFilePersonality(const AFileName: string): string;
    function FindFileTrait(const AFileName: string; const ATraitGUID: TGUID): IInterface;
    function PersonalityExists(const APersonality: string): Boolean;
  end;

  INTAPersonalityDevelopers = interface(IInterface)
    ['{765E768E-CF71-427C-AC9C-CF4BFEBCFED5}']
    function GetDeveloperNames: TStrings;
    procedure NameHit(const Name: string; Point: TPoint; const Canvas: TCanvas);
    property DeveloperNames: TStrings read GetDeveloperNames;
  end;


  IBorlandIDEServices70 = interface(IInterface)
    ['{7FD1CE92-E053-11D1-AB0B-00C04FB16FB3}']
  end;

  IBorlandIDEServices = interface(IBorlandIDEServices70)
    ['{C9E8E577-B5D8-43F3-BC84-6A734A015732}']
    function SupportsService(const Service: TGUID): Boolean;
    function GetService(const Service: TGUID): IInterface; overload;
    function GetService(const Service: TGUID; out Svc): Boolean; overload;
  end;

  { The IOTASplashScreenServices is the first service available during product
    startup, which is why it is available as a separate specific global variable.
    When this interface is created, the BorlandIDEServices interface is
    unavailable since it has yet to be initialized. }

  IOTASplashScreenServices = interface(IInterface)
    ['{1A3CDFB0-EBF6-449D-88D4-3D2991F8974F}']
    { Any IDE plugin may provide an image to be displayed on the splash screen
      as the product is initializing.  If AddPluginBitmap is called,
      AddProductBitmap should *NOT* be called or a duplicate entry will be
      displayed.  The bitmap should be 24x24 pixels with the lower-left pixel
      indicating the transparent color.  If IsUnRegistered is true, the
      caption will be painted red. LicenseStatus will be shown in parentheses
      after the caption. SKUName will be appended to the caption }
    procedure AddPluginBitmap(const ACaption: string; ABitmap: HBITMAP;
      AIsUnRegistered: Boolean = False; const ALicenseStatus: string = '';
      const ASKUName: string = '');
    { A full product personality may call AddProductBitmap in order to display
      an image on the splash screen in the same manner as AddPluginBitmap.  If
      AddProductBitmap is called, AddPluginBitmap should *NOT* be called or a
      duplicate entry will be displayed.  The bitmap should be 24x24 pixels
      with the lower-left pixel indicating the transparent color.  If
      IsUnRegistered is true, the caption will be painted red. LicenseStatus
      will be shown in parentheses after the caption. SKUName will be appended 
      to the caption.
      NOTE: AddProductBitmap should only be used by personality addins.  All
      other addins should use AddPluginBitmap.  If you call AddProductInfo and
      the IDE is not running in multi-personality mode, your Product Info will
      not actually show up in the splash screen }
    procedure AddProductBitmap(const ACaption: string; ABitmap: HBITMAP;
      IsUnRegistered: Boolean = False; const ALicenseStatus: string = '';
      const ASKUName: string = '');
    { If there is one and only one product personality installed into the IDE,
      the built-in splash screen will not be displayed.  Instead, the IDE will
      wait for the single personality to call ShowProductSplash in order to
      display that product's specific splash screen.  Even if there are multiple
      product personalities installed, each one should still call this method
      as it will be properly ignored if the IDE sees that more than one
      product personality is installed.  This keeps the product personalities
      from having to detect if they are the one and only one available. This
      bitmap should be 450x300 pixels. }
    procedure ShowProductSplash(ABitmap: HBITMAP);
    { During product startup, the IDE and any add-in or personality may wish to
      display status info for the user.  Call StatusMesage to display this
      string with the splash screen. }
    procedure StatusMessage(const StatusMessage: string);
    { A full product personality may call SetProductIcon in order to setup the
      product's icon that will be displayed on the application main form and
      the taskbar. If this is not called or there are multiple personalities
      installed, the default IDE icon is used. }
    procedure SetProductIcon(AIcon: HICON);
  end;

  { IOTAAboutBoxServices }
  IOTAAboutBoxServices120 = interface(IInterface)
    ['{1DD6FC0B-32F9-4161-A81A-9BE214F9F30D}']
    { Call AddPluginInfo to provide information specific to a particular plugin.
      The caller should save the returned index in order to later remove the
      plug-in info using RemovePluginInfo. Title is displayed in a listbox in
      which the user can select.  Description is displayed in a memo which
      should describe the plugin.  And the Image is displayed next to the
      Description when the title is selected.  The Image should follow the same
      size and rules as required for a plugin image in the splash screen.  See
      IOTASplashScreenServices.  If IsUnRegistered is true, the title will be
      painted red. LicenseStatus will be shown in a label when the title is
      selected. SKUName will also be shown in a label when the title is selected }
    function AddPluginInfo(const ATitle, ADescription: string; AImage: HBITMAP;
      AIsUnRegistered: Boolean = False; const ALicenseStatus: string = '';
      const ASKUName: string = ''): Integer;
    { AddProductInfo follows the same rules as AddPluginInfo, except for
      providing the About box Dialog title, copyright string and the extra
      about box graphic.  ACopyright should include both the product name,
      version, and copyright info.  This information is displayed at the top of
      the about box.  The Product image has the same rules as the image
      passed to AddPluginInfo. If IsUnRegistered is true, the title will be
      painted red. LicenseStatus will be shown in a label when the title is
      selected. SKUName will also be shown in a label when the title is selected.
      NOTE: AddProductInfo should only be used by personality addins.  All other
      addins should use AddPluginInfo.  If you call AddProductInfo and the IDE
      is not running in multi-personality mode, your Product Info will not
      actually show up in the About box }
    function AddProductInfo(const ADialogTitle, ACopyright, ATitle, ADescription: string;
      AAboutImage, AProductImage: HBITMAP; AIsUnRegistered: Boolean = False;
      const ALicenseStatus: string = ''; const ASKUName: string = ''): Integer;
    { When a plugin is unloaded, it should actively remove itself from the
      AboutBoxServices but specifying the index returned from AddPluginInfo }
    procedure RemovePluginInfo(Index: Integer);
    { When a product personality is unloaded, it should actively remove itself
      from the AboutBoxServices but specifying the index returned from
      AddProductInfo }
    procedure RemoveProductInfo(Index: Integer);
  end;

  { this corresponds to TAlphaFormat in Graphics.pas }
  TOTAAlphaFormat = (otaafIgnored, otaafDefined, otaafPremultiplied);

  IOTAAboutBoxServices = interface(IOTAAboutBoxServices120)
    ['{62A2F7FD-16F1-45B9-9417-A2D293D07A22}']
    { This overloaded version of AddPluginInfo adds an additional parameter that
      allows you to specify the alphaformat associated with the "AImage" bitmap }
    function AddPluginInfo(const ATitle, ADescription: string; AImage: HBITMAP;
      AIsUnRegistered: Boolean = False; const ALicenseStatus: string = '';
      const ASKUName: string = ''; AAlphaFormat: TOTAAlphaFormat = otaafIgnored): Integer;
    { This overloaded version of AddProductInfo adds additional parameters that
      allow you to specify the alphaformat associated with the "AAboutImage" and
      "AProdcutImage" bitmaps }
    function AddProductInfo(const ADialogTitle, ACopyright, ATitle, ADescription: string;
      AAboutImage, AProductImage: HBITMAP; AIsUnRegistered: Boolean = False;
      const ALicenseStatus: string = ''; const ASKUName: string = '';
      AAboutImageAlphaFormat: TOTAAlphaFormat = otaafIgnored;
      AProductImageAlphaFormat: TOTAAlphaFormat = otaafIgnored): Integer;
  end;


  { IOTAHistoryItem
    Implement this interface (and IOTANotifier) and call
    IOTAHistoryServices.AddHistoryItem to insert into the global positional or
    location history stack. }
  IOTAHistoryItem = interface(IOTANotifier)
    ['{4D97EDB8-16BE-4BD2-A624-3EE91EB4C8BC}']
    { Execute is called when this particular history item is invoked }
    procedure Execute;
    { GetItemCaption is called if the UI needs to display the history item in
      a menu or other type of list.  The implementor should return a human
      readable string that uniquely identifies this item. }
    function GetItemCaption: string;
    { IsEqual is called when inserting this item into the history stack to
      determine if this item is the same location or position as the current
      top item.  This will keep the stack from filling with adjacent duplicate
      entries. }
    function IsEqual(const Item: IOTAHistoryItem): Boolean;
  end;

  { IOTAHistoryServices
    This provides a globally available history service for managing positional
    or location history similar to an internet browser.  This service is
    typically activated by the user by selecting a back or forward arrow on a
    toolbar. }

  IOTAHistoryServices = interface(IInterface)
    ['{0EFF550D-305C-466C-94C1-1D406DF9B73A}']
    { AddHistoryItem adds the given items to the history stack at the current
      stack pointer location.  All items *after* the current location, which are
      the "forward" items, are destroyed.  CurItem is the current location you wish
      to save, and NewItem is the new location you wish to save. The current location
      is checked against the top of the stack and if it doesn't exists, it is then
      added.  The NewItem is always added.  It is done this way so that you can
      always get back to the current location }
    procedure AddHistoryItem(const CurItem, NewItem: IOTAHistoryItem);
    { GetBackwardCount returns the number of items in history stack that are
      ahead of the current stack pointer }
    function GetBackwardCount: Integer;
    { GetBackwardItem returns the indexed item in the current back "list"
      portion of the history stack }
    function GetBackwardItem(Index: Integer): IOTAHistoryItem;
    { GetForwardCount returns the number of items in history stack that are
      behind the current stack pointer }
    function GetForwardCount: Integer;
    { GetForwardItem returns the indexed item in the current forward "list"
      portion of the history stack }
    function GetForwardItem(Index: Integer): IOTAHistoryItem;
    { GetStackStatus returns whether or not the current stack pointer has items
      in the stack before and/or after its current location.  The results can be
      used to determine whether or not a forward or backward toolbar button is
      enabled.  Typically this is called internally by the IDE as it implements
      globally available toolbar buttons as part of this service.  This function
      is provided for informational purposes. }
    procedure GetStackStatus(var CanGoBack, CanGoForward: Boolean);
    { Call this Execute to cause the stack pointer to advance down or up the stack
      depending upon the parameter and call the Execute of the next item,
      if available. }
    procedure Execute(GoForward: Boolean); overload;
    { Call this Execute to force the item to be executed and to set the current
      stack pointer to this position. }
    procedure Execute(const AItem: IOTAHistoryItem); overload;
    { RemoveHistoryItem can be used to forcibly remove items from the history
      list. Typically this method is not used, however in cases where the module
      that implements this item is being unloaded, it may be necessary. There
      be no items in the list that could potentially cause control to transfer to
      a spot where there is no longer any valid code. }
    procedure RemoveHistoryItem(const Item: IOTAHistoryItem);

    property BackwardCount: Integer read GetBackwardCount;
    property BackwardItems[Index: Integer]: IOTAHistoryItem read GetBackwardItem;
    property ForwardCount: Integer read GetForwardCount;
    property ForwardItems[Index: Integer]: IOTAHistoryItem read GetForwardItem;
  end;

  IOTACompileNotifier = interface(IInterface)
    ['{C138C3D0-2806-479F-9960-950CD0B8A874}']
    procedure ProjectCompileStarted(const Project: IOTAProject; Mode: TOTACompileMode);
    procedure ProjectCompileFinished(const Project: IOTAProject; Result: TOTACompileResult);
    procedure ProjectGroupCompileStarted(Mode: TOTACompileMode);
    procedure ProjectGroupCompileFinished(Result: TOTACompileResult);
  end;

  IOTACompileServices = interface(IInterface)
    ['{68C486EF-C079-4D40-B462-2C0DD21FE342}']
    function AddNotifier(Notifier: IOTACompileNotifier): Integer;
    { Cancel background compilation. Waits for background thread
      to terminate before returning. Optionally prompt the user,
      returning True if the thread was cancelled and false if not }
    function CancelBackgroundCompile(Prompt: Boolean): Boolean;
    { Compile a list of projects. Returns crOTABackground if background
      compiling. Add IOTACompileNotifier to be informed of the result of
      background compilation }
    function CompileProjects(Projects: array of IOTAProject; CompileMode: TOTACompileMode;
      Wait, ClearMessages: Boolean): TOTACompileResult;
    { Prevent any subsequent compilation requests to the IDE from taking
      place in the background thread, regardless of settings }
    procedure DisableBackgroundCompilation;
    { Re-enable background compilation in the IDE }
    procedure EnableBackgroundCompilation;
    { Returns true if a background compile is currently running }
    function IsBackgroundCompileActive: Boolean;
    procedure RemoveNotifier(Index: Integer);
  end;

  IOTAProjectFileStorageNotifier = interface(IInterface)
    ['{D6B7B13F-F5EA-4320-BDCE-55236638BDE2}']
    { This function will return the name of your node in the project file. }
    function GetName: string;
    { Called when a project is loaded and there is a node that matches the
      result of GetName.  You may keep a reference to Node and edit the contents
      but you must free the reference when ProjectClosing is called. }
    procedure ProjectLoaded(const ProjectOrGroup: IOTAModule; const Node: IXMLNode);
    procedure CreatingProject(const ProjectOrGroup: IOTAModule);
    procedure ProjectSaving(const ProjectOrGroup: IOTAModule; const Node: IXMLNode);
    procedure ProjectClosing(const ProjectOrGroup: IOTAModule);
    property Name: string read GetName;
  end;

  IOTAProjectFileStorage = interface(IInterface)
    ['{81515027-EEED-442F-977C-8F39F53D8D0A}']
    { Called to create a section in the project file to store user data.
      ProjectOrGroup is the project file or project group that the section
      is to be added.  This project must be open at the time.  SectionName is
      the name of the new section to be added to the project xml file.  This
      name must be a valid xml node name.  Set LocalProjectFile to true if the
      section is to be stored in local project file or false for the standard
      project file. }
    function AddNewSection(const ProjectOrGroup: IOTAModule; SectionName: string;
      LocalProjectFile: Boolean): IXMLNode;
    { Call this to register an IOTANotifier. The result is the index to be
      used when calling RemoveNotifier. If <0 then an error occurred. }
    function AddNotifier(const ANotifier: IOTAProjectFileStorageNotifier): Integer;
    function GetNotifierCount: Integer;
    function GetNotifier(Index: Integer): IOTAProjectFileStorageNotifier;
    { Will return the specified node if it exists, otherwise it returns nil }
    function GetProjectStorageNode(const ProjectOrGroup: IOTAModule;
       const NodeName: string; LocalProjectFile: Boolean): IXMLNode;
    procedure RemoveNotifier(Index: Integer);
  end;

  { This notifier is deprecated. Use IOTAProjectMenuItemCreatorNotifier instead.
    It supports adding menu items for multi-selected items in the Project Manager. }
  INTAProjectMenuCreatorNotifier = interface(IOTANotifier)
    ['{8209348C-2114-439C-AD4E-BFB7049A636A}']
    { The result will be inserted into the project manager local menu. Menu
      may have child menus. }
    function AddMenu(const Ident: string): TMenuItem;
    { Return True if you wish to install a project manager menu item for this
      ident.  In cases where the project manager node is a file Ident will be
      a fully qualified file name. }
    function CanHandle(const Ident: string): Boolean;
  end;

  IOTAProjectMenuItemCreatorNotifier = interface(IOTANotifier)
    ['{CFEE5A57-2B04-4CD6-968E-1CBF8BF96522}']
    { For each menu item you wish to add to the project manager for the given
      list of idents, add an IOTAProjectManagerMenu to the ProjectManagerMenuList.
      An example of a value for IdentList is sFileContainer and the name of the
      file, look above in this file for other constants. }
    procedure AddMenu(const Project: IOTAProject; const IdentList: TStrings;
      const ProjectManagerMenuList: IInterfaceList; IsMultiSelect: Boolean);
  end;

  IOTAProjectManager = interface(IInterface)
    ['{B142EF92-0A91-4614-A72A-CE46F9C88B7B}']
    { This function is deprecated use AddMenuItemCreatorNotifier instead }
    function AddMenuCreatorNotifier(const Notifier: INTAProjectMenuCreatorNotifier): Integer; deprecated;
    function AddMenuItemCreatorNotifier(const Notifier: IOTAProjectMenuItemCreatorNotifier): Integer;
    function GetCurrentSelection(var Ident: string): IOTAProject;
    procedure RemoveMenuCreatorNotifier(Index: Integer); deprecated;
    procedure RemoveMenuItemCreatorNotifier(Index: Integer);
  end;

  { IOTATimerServices
    This provides a globally available service for timing operations and tracking performance.
    Individual timer results are tracked and persisted by a more complete performance monitor
    implementation.  See the PerfMon.pas unit for information. Timer services must be enabled
    with the BDS.exe -TS command line switch.  The default log file name is
    BDSPerformanceData.log and is located in the application data directory.  The default
    logfile name may be overriden by including the filename along with the -TS command
    line switch (no spaces) or via the property on the service interface. }

  IOTAPerformanceTimer = interface(IInterface)
    ['{7C04C7AA-A699-41AA-B6F7-C369CFF8EB5F}']
    function GetCategory: string;
    function GetDescription: string;
    function GetResults: Integer;

    { Return the category (optional) of this timer }
    property Category: string read GetCategory;
    { Return the description of this timer }
    property Description: string read GetDescription;
    { Timer results expressed in miliseconds}
    property Results: Integer read GetResults;
  end;

  IOTATimerServices = interface(IInterface)
    ['{E7D682D3-3540-4981-9ABF-160828754191}']
    { Find a timer object from a description or category }
    function FindTimer(const Description, Category: string; ActiveOnly: Boolean = true): IOTAPerformanceTimer;
    { Retrieve a timer object to get the description, category or results }
    function GetTimer(TimerID: Integer): IOTAPerformanceTimer;
    { Returns the number of individual timers for iterating all results }
    function GetTimerCount: Integer;
    { Set a marker with the elapsed time since the process was started }
    procedure MarkElapsedTime(const Description: string);
    { Begin timing a new operation.  Returned handle is used to stop timing. Category is optional }
    function StartTimer(const Description: string; const Category: string = ''): Integer;
    { Stop an active timer  }
    procedure StopTimer(TimerID: Integer); overload;
    { Alternate stop method, useful when StartTimer and StopTimer are in different places }
    procedure StopTimer(const Description, Category: string); overload;
    { Force collected data for all timers to be written to the logfile }
    procedure UpdateLogFile;
    { The name of the logfile }
    function GetLogFileName: string;
    procedure SetLogFileName(const Value: string);
    property LogFileName: string read GetLogFileName write SetLogFileName;
  end;

  IOTAHelpTrait = interface(IDispatch)
    ['{DEE36173-1597-498A-A85A-C90BFCAE9B74}']
  end;

  IOTAPersonalityHelpTrait = interface(IDispatch)
    ['{914E82DB-4123-4AA8-91D9-DB105E1FEC64}']
    procedure ShowKeywordHelp(const Keyword: WideString); safecall;
    function UnderstandsKeyword(const Keyword: WideString): WordBool; safecall;
  end;

  IOTAHelpServices = interface(IDispatch)
    ['{25F4CC12-EA93-4AEC-BC4A-DFDF427053B0}']
    procedure ShowKeywordHelp(const Keyword: WideString); safecall;
    function UnderstandsKeyword(const Keyword: WideString): WordBool; safecall;
    procedure ShowContextHelp(ContextID: Integer); safecall;
    procedure ShowTopicHelp(const Topic: WideString); safecall;
    function GetFileHelpTrait(const FileName: WideString): IOTAHelpTrait; safecall;
    function GetPersonalityHelpTrait(const Personality: WideString): IOTAPersonalityHelpTrait; safecall;
  end;

  { This is meant to be an abstract interface that describes a menu context that
    can be passed to a IOTALocalMenu-descendant's Execute method. }
  IOTAMenuContext = interface(IInterface)
    ['{378F0D38-ED5F-4128-B7D6-9D423FC1502F}']
    { Returns the identifier for this context }
    function GetIdent: string;
    { Returns the verb for this context }
    function GetVerb: string;

    property Ident: string read GetIdent;
    property Verb: string read GetVerb;
  end;

  { This is meant to be an abstract interface that describes a local menu item
    in an IDE view.  Specific views that can have their local menus customized
    will provide a descendant interface to be used for that view }
  IOTALocalMenu = interface(IOTANotifier)
    ['{83ECCBDF-939D-4F8D-B96D-A0C67ACC86EA}']
    { Returns the Caption to be used for this menu item }
    function GetCaption: string;
    { Returns the Checked state to be used for this menu item }
    function GetChecked: Boolean;
    { Returns the Enabled state to be used for this menu item }
    function GetEnabled: Boolean;
    { Returns the help context to be used for this menu item }
    function GetHelpContext: Integer;
    { Returns the Name for this menu item.  If blank, a name will be generated }
    function GetName: string;
    { Returns the parent menu for this menu item }
    function GetParent: string;
    { Returns the position of this menu item within the menu }
    function GetPosition: Integer;
    { Returns the verb associated with this menu item }
    function GetVerb: string;
    { Sets the Caption of the menu item to the specified value }
    procedure SetCaption(const Value: string);
    { Sets the Checked state of the menu item to the specified value }
    procedure SetChecked(Value: Boolean);
    { Sets the Enabled  state of the menu item to the specified value }
    procedure SetEnabled(Value: Boolean);
    { Sets the help context of the menu item to the specified value }
    procedure SetHelpContext(Value: Integer);
    { Sets the Name of the menu item to the specified value }
    procedure SetName(const Value: string);
    { Sets the Parent of the menu item to the specified value }
    procedure SetParent(const Value: string);
    { Sets the position of the menu item to the specified value }
    procedure SetPosition(Value: Integer);
    { Sets the verb associated with the menu item to the specified value }
    procedure SetVerb(const Value: string);

    property Caption: string read GetCaption write SetCaption;
    property Checked: Boolean read GetChecked write SetChecked;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property HelpContext: Integer read GetHelpContext write SetHelpContext;
    property Name: string read GetName write SetName;
    property Parent: string read GetParent write SetParent;
    property Position: Integer read GetPosition write SetPosition;
    property Verb: string read GetVerb write SetVerb;
  end;

  { This is the context used for Project Manager local menu items.  The list
    passed to IOTAProjectManagerMenu.Execute will be a list of these interfaces }
  IOTAProjectMenuContext = interface(IOTAMenuContext)
    ['{ECEC33FD-837A-46DC-A0AD-1FFEBEEA23AF}']
    { Returns the project associated with the menu item }
    function GetProject: IOTAProject;

    property Project: IOTAProject read GetProject;
  end;

  { This is a Project Manager specific local menu item }
  IOTAProjectManagerMenu = interface(IOTALocalMenu)
    ['{5E3B2F18-306E-4922-9067-3F71843C51FA}']
    { Indicates whether or not this menu item supports multi-selected items }
    function GetIsMultiSelectable: Boolean;
    { Sets this menu item's multi-selected state }
    procedure SetIsMultiSelectable(Value: Boolean);
    { Execute is called when the menu item is selected.  MenuContextList is a
      list of IOTAProjectMenuContext.  Each item in the list represents an item
      in the project manager that is selected }
    procedure Execute(const MenuContextList: IInterfaceList); overload;
    { PreExecute is called before the Execute method.  MenuContextList is a list
      of IOTAProjectMenuContext.  Each item in the list represents an item in
      the project manager that is selected }
    function PreExecute(const MenuContextList: IInterfaceList): Boolean;
    { PostExecute is called after the Execute method.  MenuContextList is a list
      of IOTAProjectMenuContext.  Each item in the list represents an item in
      the project manager that is selected }
    function PostExecute(const MenuContextList: IInterfaceList): Boolean;

    property IsMultiSelectable: Boolean read GetIsMultiSelectable write SetIsMultiSelectable;
  end;

  IOTAVersionControlNotifier = interface(IOTANotifier)
    ['{C301E578-D2AE-48EA-9EEA-AEE48D578FE7}']
    { Returns the name that is shown if more than one version control system is
      installed and the user wants to add a project to a version control system }
    function GetDisplayName: string;
    { Returns true if the file is currently managed/versioned by the version
      control system.  Under most cases "Ident" will be a fully-qualified
      filename -- however, if the user clicks on a node that is not a file (for
      example the "Contains" node for a package project), then "Ident" will be
      the name of that node }
    function IsFileManaged(const Project: IOTAProject; const IdentList: TStrings): Boolean;
    { This procedure is called when the project manager is creating its local
      menu.  The version control system may add any menu items to
      ProjectManagerMenuList that it wishes to have shown }
    procedure ProjectManagerMenu(const Project: IOTAProject; const IdentList: TStrings;
      const ProjectManagerMenuList: IInterfaceList; IsMultiSelect: Boolean);
    { Called then the user selects a menu item to add thier project to this
      version control system }
    function AddNewProject(const Project: IOTAProject): Boolean;

    property DisplayName: string read GetDisplayName;
  end;

  { IOTAVersionControlServices provides functionality for version control
    systems to plug into the IDE }
  IOTAVersionControlServices = interface(IInterface)
    ['{1BFE2647-9BFC-4084-AE3E-3E09A9179E34}']
    function AddNotifier(const ANotifier: IOTAVersionControlNotifier): Integer;
    function GetCount: Integer;
    function GetItem(const Index: Integer): IOTAVersionControlNotifier;
    procedure RemoveNotifier(Index: Integer);

    property Count: Integer read GetCount;
    property Items[const Index: Integer]: IOTAVersionControlNotifier read GetItem;
  end;

  { This class serves as a stubbed implementation of the IOTANotifier interface.
    simply statically override the methods you intend to implement and redeclare
    IOTANotifier or descendant.  The most common overrides would probably be,
    Destroyed and Modified.  Some Subsystems do *not* call all methods since in
    some cases there is not such operation. This object does not exist for
    C++Builder since these interfaces are used directly as pure virtual COM-style
    classes. }

  TNotifierObject = class(TInterfacedObject)
  protected
    { IOTANotifier }
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
  end;

  TModuleNotifierObject = class(TNotifierObject)
  protected
    { IOTAModuleNotifier }
    function CheckOverwrite: Boolean;
    procedure ModuleRenamed(const NewName: string);
    { IOTAModuleNotifier80 }
    function AllowSave: Boolean;
    function GetOverwriteFileNameCount: Integer;
    function GetOverwriteFileName(Index: Integer): string;
    procedure SetSaveFileName(const FileName: string);
  end;

  { Implements IOTAFile }
  TOTAFile = class(TInterfacedObject, IOTAFile)
    FSource: string;
    FAge: TDateTime;
  public
    constructor Create(const StringCode: String; const Age: TDateTime = -1);
    { IOTAFile }
    function GetSource: string; virtual;
    function GetAge: TDateTime; virtual;
  end;

  TOTAStringsAdapter = class(TInterfacedObject, IOTAStrings, INTAStrings)
  private
    FStrings: TStrings;
    FOwned: Boolean;
  protected
    { IOTAStrings }  
    procedure Assign(const Strings: IOTAStrings);
    function GetCount: Integer;
    function GetData(const Index: Integer): Integer;
    function GetItem(const Index: Integer): string;
    function GetName(const Index: Integer): string;
    function GetValue(const Name: string): string;
    function GetValueFromIndex(const Index: Integer): string;
    procedure SetData(const Index: Integer; Value: Integer);
    procedure SetItem(const Index: Integer; const Value: string);
    procedure SetValue(const Name, Value: string);
    procedure SetValueFromIndex(const Index: Integer; const Value: string);
    { INTAStrings }
    function GetStrings: TStrings;
  public
    constructor Create(AStrings: TStrings; AOwned: Boolean = False);
    destructor Destroy; override;
  end;

var
 (* The BorlandIDEServices global variable is initialized by the Delphi or
    C++Builder IDE.  From this interface all of the IxxxxServices interfaces
    may be queried for.  For example, in order to obtain the IOTAModuleServices
    interface, simply call the QueryInterface method with the interface
    identifier or the GUID for the IOTAModuleServices interface.  In Delphi, you
    could also use the "as" operator, however not all versions of the IDEs will
    support all the "services" interfaces.  IOTATodoServices is only supported
    in the Professional and Enterprise versions of the products.

    In Delphi;
      var
        ModuleServices: IOTAModuleServices;

      ...

      if Supports(BorlandIDEServices, IOTAModuleServices, ModuleServices) then
      begin
        ...
      end;

    or in C++Builder;

      IOTAModuleServices *ModuleServices;
      if (Supports(BorlandIDEServices, __uuidof(IOTAModuleServices), &ModuleServices))
      {
        ...
      }
  *)
  BorlandIDEServices: IBorlandIDEServices;
  SplashScreenServices: IOTASplashScreenServices;
  LibraryWizardProc: TWizardRegisterProc = nil;

procedure RegisterPackageWizard(const Wizard: IOTAWizard);

{ Convert a String into an IOTAFile for use by the IDE }
function StringToIOTAFile(const CodeString: string): IOTAFile;

{ Returns the Active Project }
function GetActiveProject: IOTAProject;

function PersonalityServices: IOTAPersonalityServices;

implementation

function StringToIOTAFile(const CodeString: string): IOTAFile;
begin
  Result := TOTAFile.Create(CodeString);
end;

function GetActiveProject: IOTAProject;
begin
  Result := nil;
  if Assigned(BorlandIDEServices) then
    Result := (BorlandIDEServices as IOTAModuleServices).GetActiveProject;
end;

procedure RegisterPackageWizard(const Wizard: IOTAWizard);
begin
  if Assigned(LibraryWizardProc) then
    LibraryWizardProc(Wizard);
end;

{ TNotifierObject }

procedure TNotifierObject.AfterSave;
begin
  // do nothing stub implementation
end;

procedure TNotifierObject.BeforeSave;
begin
  // do nothing stub implementation
end;

procedure TNotifierObject.Destroyed;
begin
  // do nothing stub implementation
end;

procedure TNotifierObject.Modified;
begin
  // do nothing stub implementation
end;

{ TOTAFile }
constructor TOTAFile.Create(const StringCode: String; const Age: TDateTime);
begin
  FSource := StringCode;
  FAge := Age;
end;

function    TOTAFile.GetSource: string;
begin
  Result := FSource;
end;

function    TOTAFile.GetAge: TDateTime;
begin
  Result := FAge;
end;

{ TModuleNotifierObject }

function TModuleNotifierObject.AllowSave: Boolean;
begin
  Result := True;
end;

function TModuleNotifierObject.CheckOverwrite: Boolean;
begin
  Result := True;
end;

function TModuleNotifierObject.GetOverwriteFileName(Index: Integer): string;
begin
  Result := '';
end;

function TModuleNotifierObject.GetOverwriteFileNameCount: Integer;
begin
  Result := 0;
end;

procedure TModuleNotifierObject.ModuleRenamed(const NewName: string);
begin
  { Do nothing }
end;

procedure TModuleNotifierObject.SetSaveFileName(const FileName: string);
begin
  { Do nothing }
end;

function PersonalityServices: IOTAPersonalityServices;
begin
  Supports(BorlandIDEServices, IOTAPersonalityServices, Result);
end;

{ TOTAStringsAdapter }

procedure TOTAStringsAdapter.Assign(const Strings: IOTAStrings);
var
  NTAStrings: INTAStrings;
  I, Index: Integer;
begin
  if Supports(Strings, INTAStrings, NTAStrings) then
    FStrings.Assign(NTAStrings.Strings)
  else
  begin
    FStrings.Clear;
    for I := 0 to Strings.Count - 1 do
    begin
      Index := FStrings.Add(Strings.Items[I]);
      FStrings.Objects[Index] := TObject(Strings.Data[I]);
    end;
  end;
end;

constructor TOTAStringsAdapter.Create(AStrings: TStrings; AOwned: Boolean = False);
begin
  inherited Create;
  FStrings := AStrings;
  FOwned := AOwned;
end;

destructor TOTAStringsAdapter.Destroy;
begin
  if FOwned then
    FStrings.Free;
  inherited Destroy;
end;

function TOTAStringsAdapter.GetCount: Integer;
begin
  Result := FStrings.Count;
end;

function TOTAStringsAdapter.GetData(const Index: Integer): Integer;
begin
  Result := Integer(FStrings.Objects[Index]);
end;

function TOTAStringsAdapter.GetItem(const Index: Integer): string;
begin
  Result := FStrings[Index];
end;

function TOTAStringsAdapter.GetName(const Index: Integer): string;
begin
  Result := FStrings.Names[Index];
end;

function TOTAStringsAdapter.GetStrings: TStrings;
begin
  Result := FStrings;
end;

function TOTAStringsAdapter.GetValue(const Name: string): string;
begin
  Result := FStrings.Values[Name];
end;

function TOTAStringsAdapter.GetValueFromIndex(const Index: Integer): string;
begin
  Result := FStrings.ValueFromIndex[Index];
end;

procedure TOTAStringsAdapter.SetData(const Index: Integer; Value: Integer);
begin
  FStrings.Objects[Index] := TObject(Value);
end;

procedure TOTAStringsAdapter.SetItem(const Index: Integer; const Value: string);
begin
  FStrings[Index] := Value;
end;

procedure TOTAStringsAdapter.SetValue(const Name, Value: string);
begin
  FStrings.Values[Name] := Value;
end;

procedure TOTAStringsAdapter.SetValueFromIndex(const Index: Integer; const Value: string);
begin
  FStrings.ValueFromIndex[Index] := Value;
end;

end.
