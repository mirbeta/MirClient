{*******************************************************}
{          MiTeC Key and Mouse Logger Component         }
{                                                       }
{         Copyright (c) 2015-2017 Michal Mutl           }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_RawInput;

interface

uses{$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils
     {$ELSE}
     Windows, SysUtils
     {$ENDIF}
     ;

const
  //WM_INPUT / wParam
  RIM_INPUT     = 0;
  RIM_INPUTSINK = 1;

  //WM_INPUT_DEVICE_CHANGE / wParam
  GIDC_ARRIVAL = 1;
  GIDC_REMOVAL = 2;

  RIM_TYPEHID      = 2;
  RIM_TYPEKEYBOARD = 1;
  RIM_TYPEMOUSE    = 0;

  RIDEV_APPKEYS      = $00000400;
  RIDEV_CAPTUREMOUSE = $00000200;
  RIDEV_DEVNOTIFY    = $00002000;
  RIDEV_EXCLUDE      = $00000010;
  RIDEV_EXINPUTSINK  = $00001000;
  RIDEV_INPUTSINK    = $00000100;
  RIDEV_NOHOTKEYS    = $00000200;
  RIDEV_NOLEGACY     = $00000030;
  RIDEV_PAGEONLY     = $00000020;
  RIDEV_REMOVE       = $00000001;

  //RAWMOUSE.usFlags
  MOUSE_ATTRIBUTES_CHANGED = $04;
  MOUSE_MOVE_RELATIVE      = $00;
  MOUSE_MOVE_ABSOLUTE      = $01;
  MOUSE_VIRTUAL_DESKTOP    = $02;

  //RAWMOUSE.usButtonFlags
  RI_MOUSE_LEFT_BUTTON_DOWN   = $0001;
  RI_MOUSE_LEFT_BUTTON_UP     = $0002;
  RI_MOUSE_MIDDLE_BUTTON_DOWN = $0010;
  RI_MOUSE_MIDDLE_BUTTON_UP   = $0020;
  RI_MOUSE_RIGHT_BUTTON_DOWN  = $0004;
  RI_MOUSE_RIGHT_BUTTON_UP    = $0008;
  RI_MOUSE_BUTTON_1_DOWN      = RI_MOUSE_LEFT_BUTTON_DOWN;
  RI_MOUSE_BUTTON_1_UP        = RI_MOUSE_LEFT_BUTTON_UP;
  RI_MOUSE_BUTTON_2_DOWN      = RI_MOUSE_RIGHT_BUTTON_DOWN;
  RI_MOUSE_BUTTON_2_UP        = RI_MOUSE_RIGHT_BUTTON_UP;
  RI_MOUSE_BUTTON_3_DOWN      = RI_MOUSE_MIDDLE_BUTTON_DOWN;
  RI_MOUSE_BUTTON_3_UP        = RI_MOUSE_MIDDLE_BUTTON_UP;
  RI_MOUSE_BUTTON_4_DOWN      = $0040;
  RI_MOUSE_BUTTON_4_UP        = $0080;
  RI_MOUSE_BUTTON_5_DOWN      = $0100;
  RI_MOUSE_BUTTON_5_UP        = $0200;
  RI_MOUSE_WHEEL              = $0400;

  //RAWKEYBOARD.Flags
  RI_KEY_BREAK = 1;
  RI_KEY_E0    = 2;
  RI_KEY_E1    = 4;
  RI_KEY_MAKE  = 0;

  //GetRawInputData / uiCommand
  RID_HEADER = $10000005;
  RID_INPUT  = $10000003;

  //GetRawInputDeviceInfo / uiCommand
  RIDI_DEVICENAME    = $20000007;
  RIDI_DEVICEINFO    = $2000000b;
  RIDI_PREPARSEDDATA = $20000005;

type
  HRAWINPUT = THandle;

  tagRAWINPUTDEVICE = record
    usUsagePage: Word;
    usUsage: Word;
    dwFlags: DWORD;
    hwndTarget: HWND;
  end;
  RAWINPUTDEVICE = tagRAWINPUTDEVICE;
  TRawInputDevice = RAWINPUTDEVICE;
  PRawInputDevice = ^TRawInputDevice;
  LPRAWINPUTDEVICE = PRawInputDevice;
  PCRAWINPUTDEVICE = PRawInputDevice;

  tagRAWINPUTDEVICELIST = record
    hDevice: THANDLE;
    dwType: DWORD;
  end;

  RAWINPUTDEVICELIST = tagRAWINPUTDEVICELIST;
  TRAWINPUTDEVICELIST = tagRAWINPUTDEVICELIST;
  PRAWINPUTDEVICELIST = ^TRAWINPUTDEVICELIST;

  tagRAWINPUTHEADER = record
    dwType: DWORD;
    dwSize: DWORD;
    hDevice: THandle;
    wParam: WPARAM;
  end;
  RAWINPUTHEADER = tagRAWINPUTHEADER;
  TRawInputHeader = RAWINPUTHEADER;
  PRawInputHeader = ^TRawInputHeader;

  tagRAWKEYBOARD = record
    MakeCode: Word;
    Flags: Word;
    Reserved: Word;
    VKey: Word;
    Message: UINT;
    ExtraInformation: ULONG;
  end;
  RAWKEYBOARD = tagRAWKEYBOARD;
  TRawKeyboard = RAWKEYBOARD;
  PRawKeyboard = ^TRawKeyboard;
  LPRAWKEYBOARD = PRawKeyboard;

  tagRAWMOUSE = record
    usFlags:  Word;
    case Integer of
      0:  (ulButtons: ULONG);
      1:  (usButtonFlags: Word;
           usButtonsData: Word;
    ulRawButtons: ULONG;
    lLastX: Longint;
    lLastY: Longint;
    ulExtraInformation: ULONG);
  end;

  RAWMOUSE = tagRAWMOUSE;
  TRAWMOUSE = tagRAWMOUSE;
  PRAWMOUSE = ^TRAWMOUSE;
  LPRAWMOUSE = ^TRAWMOUSE;

  tagRAWHID = record
    dwSizeHid: DWORD;
    dwCount: DWORD;
    bRawData: Byte;
  end;

  RAWHID = tagRAWHID;
  TRAWHID = tagRAWHID;
  PRAWHID = ^TRAWHID;
  LPRAWHID = ^TRAWHID;

  tagRAWINPUT = record
    header: RAWINPUTHEADER;
    case Integer of
      RIM_TYPEMOUSE: (mouse: RAWMOUSE);
      RIM_TYPEKEYBOARD:(keyboard: RAWKEYBOARD);
      RIM_TYPEHID: (hid: RAWHID);
  end;

  RAWINPUT = tagRAWINPUT;
  TRAWINPUT = tagRAWINPUT;
  PRAWINPUT = ^TRAWINPUT;
  LPRAWINPUT = ^TRAWINPUT;

  PPRAWINPUT = ^PRAWINPUT;

function RegisterRawInputDevices(pRawInputDevices: PCRAWINPUTDEVICE; uiNumDevices: UINT; cbSize: UINT): BOOL; stdcall; external user32;
function GetRawInputData(hRawInput: HRAWINPUT; uiCommand: UINT; pData: Pointer; var pcbSize: UINT; cbSizeHeader: UINT): UINT; stdcall; external user32;
function DefRawInputProc(paRawInput: PPRAWINPUT; nInput: integer;  cbSizeHeader: UINT): LRESULT; stdcall; external user32;
function GetRawInputBuffer(pData: PRAWINPUT; pcbSize: PUINT; cbSizeHeader: UINT): UINT; stdcall; external user32;
function GetRawInputDeviceInfo(hDevice: THandle; uiCommand: UINT; pData: Pointer; pcbSize: PUINT): UINT; stdcall; external user32 name{$IFDEF UNICODE}'GetRawInputDeviceInfoW'{$ELSE}'GetRawInputDeviceInfoA'{$ENDIF};
function GetRawInputDeviceList(pRawInputDeviceList: PRAWINPUTDEVICELIST; puiNumDevices: PUINT; cbSize: UINT): UINT; stdcall; external user32;
function GetRegisteredRawInputDevices(pRawInputDevices: PRAWINPUTDEVICE; puiNumDevices: PUINT; cbSize: UINT): UINT; stdcall; external user32;

function GET_RAWINPUT_CODE_WPARAM(wParam: WPARAM): WPARAM;
function NEXTRAWINPUTBLOCK(ptr: PRAWINPUT): PRAWINPUT;

implementation

Function GET_RAWINPUT_CODE_WPARAM(wParam: WPARAM): WPARAM;
begin
  Result:=wParam and $FF;
end;

Function RAWINPUT_ALIGN(x: Pointer): Pointer;
begin
 {$IFDEF WIN64}
 {%H-}Result:=Pointer((NativeInt(x) + SizeOf(UInt64) - 1) and not (SizeOf(UInt64) - 1));
 {$ELSE}
 {%H-}Result:=Pointer((NativeInt(x) + SizeOf(DWORD) - 1) and not (SizeOf(DWORD) - 1));
 {$ENDIF}
end;

Function NEXTRAWINPUTBLOCK(ptr: PRAWINPUT): PRAWINPUT;
begin
 {%H-}Result:=PRAWINPUT(RAWINPUT_ALIGN(Pointer(NativeInt(ptr) + ptr^.header.dwSize)));
end;

end.

