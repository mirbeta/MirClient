{*******************************************************************************
                                FastIniFile
                                -----------

                     Copyright ?2002-2012 Pieter Zijlstra

 A TIniFile replacement for INI-files larger then 64KB with support for
 quoted values and (inline) comments. It's also faster then for instant
 TMemInifile hence the 'fast' in the component name.

 E-mail: p.zylstra@hccnet.nl
 Website: http://home.hccnet.nl/p.zylstra/
 ===============================================================================
                           This software is FREEWARE
                           -------------------------

 You may freely use it in any software, including commercial software, but
 be-aware that the code is provided as-is, with no implied warranty.
 ===============================================================================

 Design-goals:
 - Support large ini files (>64K).
 - Compatible with TIniFile (and a little with TMemIniFile).
 - Preserve existing layout (leave comments and blank lines as they were).
 - Support quoted values.
     [Messages]
     SpacedMessage=" this is a message with a leading space."
 - Support inline comments, eg.
     [Initialize]
     ;Z-axis
     I109=12       ;Velocity loop scale factor
     I111=32000    ;Fatal following error in 1/16 counts
     I112=16000    ;Warning following error in 1/16 counts
 - It would be nice when it is faster then TMemIniFile.
   (PS. it is! For the app I wrote this for (in D5) it's >10 times faster,
    UseAnsiCompare was switched off for this test. With UseAnsiCompare it
    was still about 8 times faster).
   (Since D6 TMemIniFile has been improved but this one is still faster)
 - Delphi version(s) 5, 6, 7, 2005 & 2006 (and it should work on D2007).

 Notes:
 - property UseAnsiCompare:
     For full compatibilty with TIniFile it is required to use the Ansi
     versions of the compare string/text functions. But it has a very small
     negative effect on performance. If you don't need Ansi and want some
     more speed set it to False (it's True by Default).

 - Windows has no problem with finding identifiers with leading and trailing
   spaces. I did not implement this behaviour to save some CPU cycles. If for
   whatever reason you need this, you'll have to trim the Ident before calling,
   eg. AValue := ReadString(ASection, Trim(SpacedIdent), '');
   NB the keys from the file itself will always be trimmed. So when any change
   is made to the file all spaces/tabs around " = " are gone.

 Version history:
 1.00 29 mar 2002 - First public release.
 1.10 30 jun 2002 - Modified for Delphi 6
 1.20 26 oct 2002 - Fix: See TIniItems.GetValue
                  - Fix: EraseSection did not erase when the internal
                         Idents were not 'splitted' yet (ikRawIdent was
                         missing in the case statement)
                  - Added TFastIniStream.
                  - Added Fixed Date/Time writers and readers.
                  - Added ReadInlineComment.
 1.30 17 nov 2002 - Added 'Hash' value to TIniItems to make searches in long
                    sections a little faster.
                  - FSections uses THashedStringList instead of TStringList
                    when Delphi version 6 or later is used.
 1.31 26 dec 2002 - Checked for D7, no changes.
 1.40 11 feb 2003 - The SEED and MOD values, used by password functions changed
                    to properties of TFastIniFile (they were plain constants).
                  - Added e-mail and website in comments.
 1.50 25 sep 2003 - Fix: Changed SplitStrings() a little so that Idents are
                         also recognized when they contain spaces in the
                         middle of the keyword.
                  - Added new class to speed up searching/writing sections in
                    inifiles containing a large amount of sections (used to
                    be a THashedStringlist).
                  - Fix: Idents without equal sign were not handled
                         correctly. They are now handled the same as
                         is done by Windows (D7's TIniFile).
 1.60 02 oct 2003 - Rewrote TSectionItems to make searching for sections faster
                  - Added a reintroduced version of EraseSection which can
                    also remove leading comment lines from a section when
                    a section is being removed from an inifile.
                  - Added ReadStrings and WriteStrings.
 2.00 05 may 2005 - Code optimisations.
                  - Removed property InternalSortSections, it was redundant.
                  - Added Read/WriteFont.
 2.01  1 aug 2005 - Fix: SetStrings did not set FModified. It can optional
                         check the new contents to see if it is the same
                         with the current contents. If so there is no need
                         to overwrite it (reported by M. Majoor).
                  - An attempt to make it Kylix compatible.
 2.10  2 jan 2006 - Checked for D2006.
                  - Imp: Swapped all parameters in the FCompareFunc calls.
 2.11  8 jun 2007 - New: Added property FlushFileBuffers which when set to True
                         will call Windows.FlushFileBuffers when the files is
                         saved either by a direct call to UpdateFile or when
                         the class is destroyed.
 2.12  3 jul 2007 - New: Added readonly property Count to return the number of
                         of lines in the file.
                  - New: Made file content statistics available on TFastIniFile
 2.13  4 jul 2007 - Fix: Bug in AppendSection() which added a section twice
                         to the TSectionItems, after which a possible
                         EraseSection() only removed one of them and a
                         following TSectionItems.Find() returned an invalid
                         TSectionItem which could lead to exceptions and/or
                         a corrupted file (reported by S. Bansemer).
 2.20 13 apr 2008 - New: property StripQuotes which controls whether quotes from
                         values are removed or not (default is True) ... and
                  - FPC 2.2.0 compatible (thanks to Alex Yakovlev)
 2.30 24 sep 2008 - Modified for D2009.
 2.40 29 aug 2009 - Modified for D2010.
 2.41 19 sep 2009 - New: AppendSection() (thanks to Per Andersson).
                  - New: InsertBlankLine() to add blank lines between
                         existing (!) "IniItems" (thanks to Per Andersson).
                  - New: RenameSection() (thanks to Per Andersson).
 2.50 24 jan 2010 - New: Option to specify an Encoding.
                  - New: ClearSection() (thanks to Per Andersson).
                         unlike EraseSection this will only remove the section
                         items, the section itself stays in the same position.
 2.60 23 apr 2011 - Modified for DXE.
 2.70 15 jan 2012 - Modified for DXE2.
 2.71 25 may 2012 - Fix: wrong define was used that caused warnings on missing
                         FormatSettings when using DXE (reported by H. Pedersen).
*******************************************************************************}
unit FastIniFile;

//{$B-,H+,R-,T-,X+}
{H+,R-,T-,X+}

interface

{$I CompVers.inc}

{$DEFINE UseVCL}        // Use VCL classes by default, unless...
{$IFDEF LINUX}
  {$UNDEF UseVCL}       // Do not use VCL classes with Linux/Kylix.
{$ENDIF}

{$IFDEF COMPILER12_UP}
  {$DEFINE DebugFIF}    // Used for debugging (uses pascal code instead of asm}
                        //   Forced for D2009 and up so that it will use the
                        //   built-in UNICODE enabled version of some functions.
{$ENDIF}

{$IFDEF FPC}            // Do not use assembler routines in FPC.
  {$DEFINE DebugFIF}    // Used for debugging (uses pascal code instead of asm}
  {$mode delphi}        // Use Delphi parsing mode
{$ENDIF}

uses
{$IFDEF LINUX}
  Libc,
  Types,
{$ELSE}
  Windows,
  {$IFDEF DELPHI6_UP}
    {$IFNDEF FPC}
      Types,
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
  SysUtils, Classes, IniFiles, TypInfo {$IFDEF UseVCL}, Graphics{$ENDIF}, RTLConsts;

const
  FIXED_DS       = '-';
  FIXED_DATE     = 'dd' + FIXED_DS + 'mm' + FIXED_DS + 'yyyy';
  FIXED_TS       = ':';
  FIXED_TIME     = 'hh' + FIXED_TS + 'nn' + FIXED_TS + 'ss';
  FIXED_DATETIME = FIXED_DATE + ' ' + FIXED_TIME;

  MAXBUCKETS     = 256;

  PASSWORD_SEED  = $41;
  PASSWORD_MOD   = $07;

  MaxListSize = MaxInt div (SizeOf(Pointer) * 4);

type
  TIniKeyType = (ikIdent, ikEmptyIdent, ikRawIdent, ikSection, ikComment, ikBlank);
  TInsertPosition = (ipAppend, ipBeforeSection, ipAfterSection, ipBeforeIdent, ipAfterIdent);
  TInsertMode = (imCanCreateKey, imOverwriteExisting);
  TInsertModes = set of TInsertMode;

  TCompareFunc = function(const S1, S2: string): Boolean;
  THashOfFunc = function(const S: string): Cardinal;

  PIniItem = ^TIniItem;
  TIniItem = record
    FKeyType: TIniKeyType;
    FIdentHash: Cardinal;
    FIdentStr: string;
    FValueStr: string;
  end;

  PPIniItems = ^TPIniItems;
  TPIniItems = array[0..MaxListSize-1] of PIniItem;

  PIdentBuckets = ^TIdentBuckets;
  TIdentBuckets = array[0..MAXBUCKETS-1] of PPIniItems;

  PPSectionItem = ^PSectionItem;
  PSectionItem = ^TSectionItem;
  TSectionItem = record
    FNext: PSectionItem;
    FIdentBuckets: PIdentBuckets;
    FIndex: Integer;    // The Index of the section within TIniItems
    FName: string;
  end;


{ TSectionItems }

  TSectionItems = class(TObject)
  private
    FBuckets: array[0..MAXBUCKETS-1] of PSectionItem;
    FCaseSensitive: Boolean;
    FCompareFunc: TCompareFunc;
    FHashAll: Boolean;
    FHashOfFunc: THashOfFunc;
    procedure AddIniItem(PS: PSectionItem; IniItem: PIniItem);
    procedure ClearIdentBuckets(PB: PIdentBuckets);
    procedure Delete(const PPS: PPSectionItem); overload;
    procedure FreeAndNilAllIdentBuckets;
    procedure FreeAndNilIdentBuckets(var PB: PIdentBuckets);
  protected
    function Find(const Key: string): PPSectionItem;
    procedure UpdateIndexes(Index, Delta: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const S: string; Index: Integer): PSectionItem;
    procedure Clear;
    procedure Delete(const S: string); overload;
  end;


{ TStatistics class }

  TStatistics = class(TObject)
  private
    FBlankLines: Integer;
    FComments: Integer;
    FIdents: Integer;
    FSections: Integer;
  protected
    procedure Clear;
  public
    property BlankLines: Integer read FBlankLines;
    property Comments: Integer read FComments;
    property Idents: Integer read FIdents;
    property Sections: Integer read FSections;
  end;


{ TIniItems class }

  PIniItemList = ^TIniItemList;
  TIniItemList = array[0..MaxListSize-1] of TIniItem;

  TIniItems = class(TObject)
  private
    FCount: Integer;
    FCapacity: Integer;
    FCompareFunc: TCompareFunc;
    FDirtyHash: Boolean;
    FFlushFileBuffers: Boolean;
    FHashAll: Boolean;
    FHashOfFunc: THashOfFunc;
    FLastSectionItem: PSectionItem;
    FList: PIniItemList;
    FSections: TSectionItems;
    FStatistics: TStatistics;
    function Get(Index: Integer): string;
    function GetItem(Index: Integer): PIniItem;
    function GetName(Index: Integer): string;
    function GetValue(Index: Integer): string;
    procedure Grow;
    procedure HashIdents(PS: PSectionItem);
    procedure InsertItem(Index: Integer; const S: string);
    procedure Put(Index: Integer; const S: string); virtual;
    procedure SetCompareFunc(const Value: TCompareFunc);
    procedure SetHashAll(const Value: Boolean);
    procedure SetHashOfFunc(const Value: THashOfFunc);
    procedure SetValue(Index: Integer; const S: string);
    function SplitStrings(var Ident, Value: string): Boolean;
  protected
    function AppendIdent(const Section, Ident: string): Integer;
    function AppendSection(const Section: string): PSectionItem;
    procedure EraseSection(const Section: string; RemoveLeadingCommentLines: Boolean; const OnlyItems : Boolean);
    procedure Error(const Msg: string; Data: Integer); overload;
    procedure Error(Msg: PResStringRec; Data: Integer); overload;
    function FindIniItem(const Section, Ident: string; Force: Boolean): PIniItem;
    function FindSectionItem(const Section: string; Force: Boolean): PSectionItem;
    function GetCapacity: Integer;
    function GetCount: Integer;
    function GetTextStr: string;
    function RenameSection(const OldName, NewName: string): Boolean;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetTextStr(const Value: string);
    property CompareFunc: TCompareFunc read FCompareFunc write SetCompareFunc;
    property FlushFileBuffers: Boolean read FFlushFileBuffers write FFlushFileBuffers;
    property HashOfFunc: THashOfFunc read FHashOfFunc write SetHashOfFunc;
    property Sections: TSectionItems read FSections;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const S: string): Integer;
    procedure Clear;
    procedure Delete(Index: Integer); overload;
    procedure Delete(Index, N: Integer); overload;
    function IndexOfIdent(const Section, Ident: string; Force: Boolean): Integer;
    function IndexOfSection(const Section: string; Force: Boolean): Integer;
    procedure Insert(Index: Integer; const S: string);
    procedure LoadFromFile(const AFileName: string); overload; virtual;
{$IFDEF COMPILER12_UP}
    procedure LoadFromFile(const AFileName: string; Encoding: TEncoding); overload; virtual;
{$ENDIF}
    procedure LoadFromStream(Stream: TStream); overload; virtual;
{$IFDEF COMPILER12_UP}
    procedure LoadFromStream(Stream: TStream; Encoding: TEncoding); overload; virtual;
{$ENDIF}
    procedure SaveToFile(const AFileName: string); overload; virtual;
{$IFDEF COMPILER12_UP}
    procedure SaveToFile(const AFileName: string; Encoding: TEncoding); overload; virtual;
{$ENDIF}
    procedure SaveToStream(Stream: TStream); overload; virtual;
{$IFDEF COMPILER12_UP}
    procedure SaveToStream(Stream: TStream; Encoding: TEncoding); overload; virtual;
{$ENDIF}
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
    property HashAll: Boolean read FHashAll write SetHashAll;
    property Items[Index: Integer]: PIniItem read GetItem;
    property Names[Index: Integer]: string read GetName;
    property Statistics: TStatistics read FStatistics;
    property Strings[Index: Integer]: string read Get write Put; default;
    property Text: string read GetTextStr write SetTextStr;
    property Values[Index: Integer]: string read GetValue write SetValue;
  end;


{ TFastIniFile class }

  TFastIniFile = class(TCustomIniFile)
  private
    FAllowBlankLine: Boolean;
    FCaseSensitive: Boolean;
{$IFDEF COMPILER12_UP}
    FEncoding: TEncoding;
{$ENDIF}
    FIniItems: TIniItems;
    FInlineCommentsEnabled: Boolean;
    FLoaded: Boolean;
    FModified: Boolean;
    FPasswordSeed: Integer;
    FPasswordMod: Integer;
    FUseAnsiCompare: Boolean;
    FStripQuotes: Boolean;
    function GetCount: Integer;
    function GetFlushFileBuffers: Boolean;
    function GetHashAll: Boolean;
    function GetStatistics: TStatistics;
    function IncludeLeadingSemicolon(const Comment: string): string;
    procedure SetCaseSensitive(const Value: Boolean);
    procedure SetFlushFileBuffers(const Value: Boolean);
    procedure SetHashAll(const Value: Boolean);
    procedure SetUseAnsiCompare(Value: Boolean);
  protected
    procedure Clear; virtual;
    function CompareStrings(S1, S2: TStrings): Integer; virtual;
    procedure FlushBuffers; virtual;
    procedure LoadValues; virtual;
    property Loaded: Boolean read FLoaded write FLoaded;
    property Modified: Boolean read FModified write FModified;
  public
    constructor Create(const AFileName: string); {$IFDEF FPC} reintroduce {$ENDIF} overload;
{$IFDEF COMPILER12_UP}
    constructor Create(const AFileName: string; Encoding: TEncoding); {$IFDEF FPC} reintroduce {$ENDIF} overload;
{$ENDIF}
    destructor Destroy; override;
    function AppendSection(const Section: string): Boolean;
    procedure DeleteKey(const Section, Ident: string); override;
    procedure ClearSection(const Section: string);
    procedure EraseSection(const Section: string); overload; override;
    procedure EraseSection(const Section: string; RemoveLeadingCommentLines: Boolean); reintroduce; overload;
    procedure GetStrings(List: TStrings);
    procedure ReadSection(const Section: string; Strings: TStrings); override;
    procedure ReadSections(Strings: TStrings); override;
    procedure ReadSectionValues(const Section: string; Strings: TStrings); override;
    procedure Release;
    procedure Rename(const AFileName: string; Reload: Boolean);
    function RenameSection(const OldName, NewName: string): Boolean;
    function SectionExists(const Section: string): Boolean; {$IFDEF FPC} override; {$ENDIF}
    procedure SetStrings(List: TStrings; ForceUpdate: Boolean = True);
    procedure UpdateFile; override;
    function ValueExist(const Section, Ident: string): Boolean;

    // Override the basic readers and writers
    function ReadString(const Section, Ident, Default: string): string; override;
    procedure WriteString(const Section, Ident, Value: string); override;
    // And the rest of the Readers/Writers are inherited from TCustomIniFile.
    // ...

    // Controlling comments within the ini-file.
    function InlineComment(const Section, Ident, Comment: string;
      Where: Integer; Mode: TInsertModes): Boolean;
    function InsertComment(const Section, Ident, Comment: string;
      Where: TInsertPosition; Mode: TInsertModes): Boolean;
    function ReadInlineComment(const Section, Ident, Default: string): string;

    // Just a convenient function with fewer parameters that can be used to
    // make it more obvious that you want to insert a blank line than calling
    // InsertComment with an empty Comment-string.
    function InsertBlankLine(const Section, Ident: string; Where: TInsertPosition): Boolean;

    // Some of my own frequently used stuff.
    function  ReadBoolean(const Section, Ident: string; Default: Boolean): Boolean;
    procedure WriteBoolean(const Section, Ident: string; Value: Boolean);
{$IFDEF UseVCL}
    function  ReadFont(const Section, Ident: string; const Default: TFont): TFont;
    procedure WriteFont(const Section, Ident: string; const Value: TFont);
{$ENDIF}
    procedure WriteIntHex2(const Section, Ident: string; Value: LongInt);
    procedure WriteIntHex3(const Section, Ident: string; Value: LongInt);
    procedure WriteIntHex4(const Section, Ident: string; Value: LongInt);
    procedure WriteIntHex8(const Section, Ident: string; Value: LongInt);
    function  ReadReal(const Section, Ident: string; Default: Extended): Extended;
    procedure WriteReal(const Section, Ident: string; Value: Extended);
    procedure WriteRealF(const Section, Ident: string; Value: Extended; MinDecimals: Integer);
    function  ReadPoint(const Section, Ident: string; const Default: TPoint): TPoint;
    procedure WritePoint(const Section, Ident: string; const Value: TPoint);
    function  ReadRect(const Section, Ident: string; const Default: TRect): TRect;
    procedure WriteRect(const Section, Ident: string; const Value: TRect);
    function  ReadSimplePassword(const Section, Ident: string; Default: string): string;
    procedure WriteSimplePassword(const Section, Ident: string; Value: string);
    procedure ReadStrings(const Section, Ident: string; S: TStrings);
    procedure WriteStrings(const Section, Ident: string; S: TStrings);

    // I don't like the system dependecies of the original functions,
    // next Date/Time functions use a fixed lay-out
    function  ReadFixedDate(const Section, Ident: string; Default: TDateTime): TDateTime;
    function  ReadFixedDateTime(const Section, Ident: string; Default: TDateTime): TDateTime;
    function  ReadFixedTime(const Section, Ident: string; Default: TDateTime): TDateTime;
    procedure WriteFixedDate(const Section, Ident: string; Value: TDateTime);
    procedure WriteFixedDateTime(const Section, Ident: string; Value: TDateTime);
    procedure WriteFixedTime(const Section, Ident: string; Value: TDateTime);

    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property Count: Integer read GetCount;
{$IFDEF COMPILER12_UP}
    property Encoding: TEncoding read FEncoding write FEncoding;
{$ENDIF}
    property FlushFileBuffers: Boolean read GetFlushFileBuffers write SetFlushFileBuffers;
    property HashAll: Boolean read GetHashAll write SetHashAll;
    property InlineCommentsEnabled: Boolean read FInlineCommentsEnabled write FInlineCommentsEnabled;
    property PasswordSeed: Integer read FPasswordSeed write FPasswordSeed;
    property PasswordMod: Integer read FPasswordMod write FPasswordMod;
    property Statistics: TStatistics read GetStatistics;
    property StripQuotes: Boolean read FStripQuotes write FStripQuotes;
    property UseAnsiCompare: Boolean read FUseAnsiCompare write SetUseAnsiCompare;
  end;


{ TFastIniStream class }

  TFastIniStream = class(TFastIniFile)
  private
    FStream: TStream;
  protected
    procedure FlushBuffers; override;
    procedure LoadValues; override;
  public
    constructor Create(Stream: TStream); overload;
  end;

function PointToStr(const P: TPoint): string;
function StrToPoint(const S: string; const Default: TPoint): TPoint;
function RectToStr(const R: TRect): string;
function StrToRect(const RectStr: string; const Default: TRect): TRect;
{$IFDEF UseVCL}
function StyleToStr(const Styles: TFontStyles): string;
function StrToStyle(const StyleStr: string): TFontStyles;
function FontToStr(const F: TFont): string;
function StrToFont(const FontStr: string; const Default: TFont): TFont;
{$ENDIF}
function StrToDateDef(const Value: string; Default: TDateTime): TDateTime;
function StrToTimeDef(const Value: string; Default: TDateTime): TDateTime;

implementation

uses
{$IFDEF LINUX}
  RTLConsts;
{$ELSE}
  {$IFDEF DELPHI6_UP}
    RTLConsts;
  {$ELSE}
    Consts;
  {$ENDIF}
{$ENDIF}

{$IFNDEF FPC}
type
  PtrInt = Integer;
  PtrUInt = Cardinal;
{$ENDIF}

var
  UpperCaseLookUp: array[Char] of Char;
  AnsiUpperCaseLookUp: array[Char] of Char;

{$IFDEF DebugFIF}
  // uses the RTL version
{$ELSE}
  // A SameText look-a-like based on Aleksandr Sharahov' function
  // CompareTextShaAsm3() found at the FastCode project.
function SameText(const S1, S2: string): Boolean;
asm
        test  eax, eax       // if S1 = nil then return(False)
        jz    @nil1
        test  edx, edx       // if S2 = nil then return(False)
        jz    @nil2
        mov   ecx, [eax-4]
        cmp   ecx, [edx-4]
        je    @lenok         // if Length(S1) <> Length (S2) then return(False)

@nil2:  xor   eax, eax
@nil1:  ret

@lenok: push  edi
        push  ebx
        xor   ebx, ebx
        mov   edi, eax
        sub   ebx, ecx

        sub   edi, ebx
        sub   edx, ebx

@loop:  mov   eax, [ebx+edi]
        mov   ecx, [ebx+edx]
        cmp   eax, ecx
        jne   @byte0
@same:  add   ebx, 4
        jl    @loop

@len:   mov   al, 1
        pop   ebx
        pop   edi
        ret

@loop2: mov   eax, [ebx+edi]
        mov   ecx, [ebx+edx]
        cmp   eax, ecx
        je    @same

@byte0: cmp   al, cl
        je    @byte1

        and   eax, $FF
        and   ecx, $FF
        sub   eax, 'a'
        sub   ecx, 'a'
        cmp   al, 'z'-'a'
        ja    @up0a
        sub   eax, 'a'-'A'
@up0a:  cmp   cl, 'z'-'a'
        ja    @up0c
        sub   ecx, 'a'-'A'
@up0c:  sub   eax, ecx
        jnz   @done

        mov   eax, [ebx+edi]
        mov   ecx, [ebx+edx]

@byte1: cmp   ah, ch
        je    @byte2

        and   eax, $FF00
        and   ecx, $FF00
        sub   eax, 'a'*256
        sub   ecx, 'a'*256
        cmp   ah, 'z'-'a'
        ja    @up1a
        sub   eax, ('a'-'A')*256
@up1a:  cmp   ch, 'z'-'a'
        ja    @up1c
        sub   ecx, ('a'-'A')*256
@up1c:  sub   eax, ecx
        jnz   @done

        mov   eax, [ebx+edi]
        mov   ecx, [ebx+edx]

@byte2: add   ebx, 2
        jnl   @len2
        shr   eax, 16
        shr   ecx, 16
        cmp   al, cl
        je    @byte3

        and   eax, $FF
        and   ecx, $FF
        sub   eax, 'a'
        sub   ecx, 'a'
        cmp   al, 'z'-'a'
        ja    @up2a
        sub   eax, 'a'-'A'
@up2a:  cmp   cl, 'z'-'a'
        ja    @up2c
        sub   ecx, 'a'-'A'
@up2c:  sub   eax, ecx
        jnz   @done

        movzx eax, word ptr [ebx+edi]
        movzx ecx, word ptr [ebx+edx]

@byte3: cmp   ah, ch
        je    @byte4

        and   eax, $FF00
        and   ecx, $FF00
        sub   eax, 'a'*256
        sub   ecx, 'a'*256
        cmp   ah, 'z'-'a'
        ja    @up3a
        sub   eax, ('a'-'A')*256
@up3a:  cmp   ch, 'z'-'a'
        ja    @up3c
        sub   ecx, ('a'-'A')*256
@up3c:  sub   eax, ecx
        jnz   @done

@byte4: add   ebx, 2
        jl    @loop2

@len2:  mov   al, 1
        pop   ebx
        pop   edi
        ret

@done:  xor   eax, eax
        pop   ebx
        pop   edi
end;
{$ENDIF}

{$IFDEF DebugFIF}
  // uses the RTL version
{$ELSE}
  // An AnsiSameText look-a-like based on Aleksandr Sharahov' function
  // CompareTextShaAsm3() found at the FastCode project.
function AnsiSameText(const S1, S2: string): Boolean;
asm
        test  eax, eax       // if S1 = nil then return(False)
        jz    @nil1
        mov   ecx, [eax-4]
        test  edx, edx       // if S2 = nil then return(False)
        jz    @nil2
        cmp   ecx, [edx-4]
        je    @lenok         // if Length(S1) <> Length (S2) then return(False)

@nil2:  xor   eax, eax
@nil1:  ret

@lenok: push  edi
        push  ebx
        xor   ebx, ebx
        mov   edi, eax
        sub   ebx, ecx

        sub   edi, ebx
        sub   edx, ebx

@loop:  mov   eax, [ebx+edi]
        mov   ecx, [ebx+edx]
        cmp   eax, ecx
        jne   @byte0
@same:  add   ebx, 4
        jl    @loop

@len:   mov   al, 1
        pop   ebx
        pop   edi
        ret

@loop2: mov   eax, [ebx+edi]
        mov   ecx, [ebx+edx]
        cmp   eax, ecx
        je    @same

@byte0: cmp   al, cl
        je    @byte1

        and   eax, $FF
        and   ecx, $FF
        mov   al, byte ptr [eax+AnsiUpperCaseLookup]
        mov   cl, byte ptr [ecx+AnsiUpperCaseLookup]
        sub   eax, ecx
        jnz   @done

        mov   eax, [ebx+edi]
        mov   ecx, [ebx+edx]

@byte1: cmp   ah, ch
        je    @byte2

        shr   eax, 8
        shr   ecx, 8

        and   eax, $FF
        and   ecx, $FF
        mov   al, byte ptr [eax+AnsiUpperCaseLookup]
        mov   cl, byte ptr [ecx+AnsiUpperCaseLookup]
        sub   eax, ecx
        jnz   @done

        mov   eax, [ebx+edi]
        mov   ecx, [ebx+edx]

@byte2: add   ebx, 2
        jnl   @len2
        shr   eax, 16
        shr   ecx, 16
        cmp   al, cl
        je    @byte3

        and   eax, $FF
        and   ecx, $FF
        mov   al, byte ptr [eax+AnsiUpperCaseLookup]
        mov   cl, byte ptr [ecx+AnsiUpperCaseLookup]
        sub   eax, ecx
        jnz   @done

        movzx eax, word ptr [ebx+edi]
        movzx ecx, word ptr [ebx+edx]

@byte3: cmp   ah, ch
        je    @byte4

        shr   eax, 8
        shr   ecx, 8

        and   eax, $FF
        and   ecx, $FF
        mov   al, byte ptr [eax+AnsiUpperCaseLookup]
        mov   cl, byte ptr [ecx+AnsiUpperCaseLookup]
        sub   eax, ecx
        jnz   @done

@byte4: add   ebx, 2
        jl    @loop2

@len2:  mov   al, 1
        pop   ebx
        pop   edi
        ret

@done:  xor   eax, eax
        pop   ebx
        pop   edi
end;
{$ENDIF}

{$IFDEF DebugFIF}
function SameStr(const S1, S2: string): Boolean;
begin
  Result := (S1 = S2);
end;
{$ELSE}
function SameStr(const S1, S2: string): Boolean;
asm
        call  System.@LStrCmp;
        setz  al
end;
{$ENDIF}

function HashOfStr(const S: string): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(S) do
    Result := ((Result shl 2) or (Result shr (SizeOf(Result) * 8 - 2))) xor
      Ord(S[I]);
end;

function HashOfText(const S: string): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(S) do
    Result := ((Result shl 2) or (Result shr (SizeOf(Result) * 8 - 2))) xor
      Ord(UpperCaseLookUp[S[I]]);
end;

function HashOfAnsi(const S: string): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(S) do
    Result := ((Result shl 2) or (Result shr (SizeOf(Result) * 8 - 2))) xor
      Ord(AnsiUpperCaseLookUp[S[I]]);
end;

function PointToStr(const P: TPoint): string;
begin
  with P do
    Result := Format('%d,%d', [X, Y]);
end;

function StrToPoint(const S: string; const Default: TPoint): TPoint;
var
  I: Integer;
begin
  I := Pos(',', S);
  if I = 0 then  // Assume only X is specified
  begin
    Result.X := StrToIntDef(S, Default.X);
    Result.Y := Default.Y;
  end
  else
  begin
    Result.X := StrToIntDef(Copy(S, 1, I - 1), Default.X);
    Result.Y := StrToIntDef(Copy(S, I + 1, 255), Default.Y);
  end;
end;

function RectToStr(const R: TRect): string;
begin
  with R do
    Result := Format('%d,%d,%d,%d', [Left, Top, Right, Bottom]);
end;

function StrToRect(const RectStr: string; const Default: TRect): TRect;
var
  S: string;
  I: Integer;
begin
  Result := Default;
  S := RectStr;
  I := Pos(',', S);
  if I > 0 then
  begin
    Result.Left := StrToIntDef(Trim(Copy(S, 1, I - 1)), Default.Left);
    Delete(S, 1, I);
    I := Pos(',', S);
    if I > 0 then
    begin
      Result.Top := StrToIntDef(Trim(Copy(S, 1, I - 1)), Default.Top);
      Delete(S, 1, I);
      I := Pos(',', S);
      if I > 0 then
      begin
        Result.Right := StrToIntDef(Trim(Copy(S, 1, I - 1)), Default.Right);
        Delete(S, 1, I);
        Result.Bottom := StrToIntDef(Trim(S), Default.Bottom);
      end;
    end;
  end;
end;

{$IFDEF UseVCL}
function StyleToStr(const Styles: TFontStyles): string;
var
  Style: TFontStyle;
begin
  Result := '[';
  for Style := Low(Style) to High(Style) do
  begin
    if Style in Styles then
    begin
      if Length(Result) > 1 then
        Result := Result + ',';
      Result := Result + GetEnumname(TypeInfo(TFontStyle), Ord(Style));
    end;
  end;
  Result := Result + ']';
end;

function StrToStyle(const StyleStr: string): TFontStyles;
var
  I: Integer;
  S: string;
  SL: TStringlist;
  Style: TFontStyle;
begin
  Result := [];

  S := StyleStr;
  if (Length(S) > 2) and (S[1] = '[') and (S[Length(S)] = ']') then
    S := Copy(S, 2, Length(S) - 2)
  else
    Exit;

  SL := TStringlist.Create;
  try
    SL.CommaText := S;
    for I := 0 To SL.Count - 1 do
      try
        Style := TFontStyle(GetEnumValue(TypeInfo(TFontStyle), SL[I]));
        Include(Result, Style);
      except
      end;
  finally
    SL.Free;
  end;
end;

function FontToStr(const F: TFont): string;
begin
  with F do
    Result := Format('%s,%d,%s,%s,%d,%d', [Name, Size, StyleToStr(Style),
      ColorToString(Color), Ord(Pitch), Charset]);
end;

function StrToFont(const FontStr: string; const Default: TFont): TFont;
var
  S: string;
  I: Integer;
begin
  Result := Default;
  S := FontStr;
  I := Pos(',', S);
  if I > 0 then
  begin
    Result.Name := Trim(Copy(S, 1, I - 1));
    Delete(S, 1, I);
    I := Pos(',', S);
    if I > 0 then
    begin
      Result.Size := StrToIntDef(Trim(Copy(S, 1, I - 1)), Default.Size);
      Delete(S, 1, I);
      I := Pos(']', S);
      if I > 0 then
      begin
        Result.Style := StrToStyle(Trim(Copy(S, 1, I)));
        Delete(S, 1, I + 1);
        I := Pos(',', S);
        if I > 0 then
        begin
          Result.Color := StringToColor(Trim(Copy(S, 1, I - 1)));
          Delete(S, 1, I);
          I := Pos(',', S);
          if I > 0 then
          begin
            Result.Pitch := TFontPitch(StrToIntDef(Trim(Copy(S, 1, I - 1)),
              Ord(Default.Pitch)));
            Delete(S, 1, I);
            Result.CharSet := StrToIntDef(Trim(S), Default.CharSet);
          end;
        end;
      end;
    end;
  end;
end;
{$ENDIF}

function StrToDateDef(const Value: string; Default: TDateTime): TDateTime;
var
  ActDS: Char;
  ActSDF: string;
begin
  try
    ActDS  := FormatSettings.DateSeparator;
    ActSDF := FormatSettings.ShortDateFormat;
    try
      FormatSettings.DateSeparator   := FIXED_DS;
      FormatSettings.ShortDateFormat := FIXED_DATE;
      Result := StrToDate(Value);
    finally
      FormatSettings.DateSeparator   := ActDS;
      FormatSettings.ShortDateFormat := ActSDF;
    end;
  except
    Result := Default;
  end;
end;

function StrToTimeDef(const Value: string; Default: TDateTime): TDateTime;
var
  ActTS: Char;
begin
  try
    ActTS := FormatSettings.TimeSeparator;
    try
      FormatSettings.TimeSeparator := FIXED_TS;
      Result := StrToTime(Value);
    finally
      FormatSettings.TimeSeparator := ActTS;
    end;
  except
    Result := Default;
  end;
end;

function FindFirstComment(const S: String): Integer;
var
  I: Integer;
  InQuotes: Boolean;
begin
  Result := 0;
  InQuotes := False;
  for I := 1 to Length(S) do
    case S[I] of
      '"': InQuotes := not InQuotes;
      ';': if not InQuotes then
           begin
             Result := I;
             Exit;
           end;
    end;
end;

{ - TSectionItems - }

constructor TSectionItems.Create;
begin
  inherited Create;
  FCompareFunc := AnsiSameText;
  FHashOfFunc := HashOfAnsi;
end;

destructor TSectionItems.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TSectionItems.Add(const S: string; Index: Integer): PSectionItem;
var
  Hash: Cardinal;
begin
  New(Result);
  Result^.FIdentBuckets := nil;
  Result^.FIndex := Index;
  Result^.FName := S;
  Hash := FHashOfFunc(S) mod MAXBUCKETS;
  Result^.FNext := FBuckets[Hash];
  FBuckets[Hash] := Result;
end;

procedure TSectionItems.AddIniItem(PS: PSectionItem; IniItem: PIniItem);
const
  Delta = 2;
var
  L: PtrUInt;
  P: ^PPIniItems;
begin
  if PS <> nil then
  begin
    P := @PS.FIdentBuckets[IniItem.FIdentHash mod MAXBUCKETS];
    if P^ = nil then
    begin
      GetMem(P^, Delta * SizeOf(PIniItem));
      P^[0] := nil;
    end;
    {$IFDEF FPC}{$IFDEF ENDIAN_LITTLE} {$HINTS OFF} {$ENDIF}{$ENDIF}
      // Note: P^[0] is used as a "Cardinal" to hold the count of items.
      L := PtrUInt(P^[0]);
      Inc(L);
      if (L and (Delta - 1)) = 0 then
        ReallocMem(P^, (L + Delta) * SizeOf(PIniItem));
      P^[0] := PIniItem(L);
      P^[L] := IniItem;
    {$IFDEF FPC}{$IFDEF ENDIAN_LITTLE} {$HINTS ON} {$ENDIF}{$ENDIF}
  end;
end;

procedure TSectionItems.Clear;
var
  I: Integer;
  P, N: PSectionItem;
begin
  for I := 0 to MAXBUCKETS - 1 do
  begin
    P := FBuckets[I];
    while P <> nil do
    begin
      N := P^.FNext;
      if P.FIdentBuckets <> nil then
        FreeAndNilIdentBuckets(P.FIdentBuckets);
      Dispose(P);
      P := N;
    end;
    FBuckets[I] := nil;
  end;
end;

procedure TSectionItems.ClearIdentBuckets(PB: PIdentBuckets);
var
  I: Integer;
begin
  for I := 0 to MAXBUCKETS - 1 do
  begin
    FreeMem(PB^[I]);
    PB^[I] := nil;
  end;
end;

procedure TSectionItems.Delete(const PPS: PPSectionItem);
var
  P: PSectionItem;
  Prev: PPSectionItem;
begin
  Prev := PPS;
  P := Prev^;
  if P <> nil then
  begin
    Prev^ := P^.FNext;
    if P.FIdentBuckets <> nil then
      FreeAndNilIdentBuckets(P.FIdentBuckets);
    Dispose(P);
  end;
end;

procedure TSectionItems.Delete(const S: string);
begin
  Delete(Find(S));
end;

function TSectionItems.Find(const Key: string): PPSectionItem;
begin
  Result := @FBuckets[FHashOfFunc(Key) mod MAXBUCKETS];

  while Result^ <> nil do
    if FCompareFunc(Key, Result^.FName) then
      Exit
    else
      Result := @Result^.FNext;
end;

procedure TSectionItems.FreeAndNilAllIdentBuckets;
var
  I: Integer;
  P, N: PSectionItem;
begin
  for I := 0 to MAXBUCKETS - 1 do
  begin
    P := FBuckets[I];
    while P <> nil do
    begin
      N := P^.FNext;
      if P.FIdentBuckets <> nil then
        FreeAndNilIdentBuckets(P.FIdentBuckets);
      P := N;
    end;
  end;
end;

procedure TSectionItems.FreeAndNilIdentBuckets(var PB: PIdentBuckets);
begin
  if PB <> nil then
  begin
    ClearIdentBuckets(PB);
    Dispose(PB);
    PB := nil;
  end;
end;

procedure TSectionItems.UpdateIndexes(Index, Delta: Integer);
var
  I: Integer;
  P: PSectionItem;
begin
  for I := 0 to MAXBUCKETS - 1 do
  begin
    P := FBuckets[I];
    while P <> nil do
    begin
      with P^ do
        if FIndex > Index then
          FIndex := FIndex + Delta;
      P := P^.FNext;
    end;
  end;
end;


{ - TStatistics - }

procedure TStatistics.Clear;
begin
  FBlankLines := 0;
  FComments := 0;
  FIdents := 0;
  FSections := 0;
end;


{ - TIniItems - }

constructor TIniItems.Create;
begin
  FSections := TSectionItems.Create;
  FStatistics := TStatistics.Create;
  FLastSectionItem := nil;
end;

destructor TIniItems.Destroy;
begin
  FStatistics.Free;
  FSections.Free;
  if FCount <> 0 then Finalize(FList^[0], FCount);
  FCount := 0;
  SetCapacity(0);
  inherited Destroy;
end;

function TIniItems.Add(const S: string): Integer;
begin
  Result := FCount;
  InsertItem(Result, S);
end;

function TIniItems.AppendIdent(const Section, Ident: string): Integer;
var
  I: Integer;
  P: PSectionItem;
begin
  // New item is inserted right after last found "Ident=Value" (or "Ident").
  P := FindSectionItem(Section, True);
  Result := P^.FIndex;
  for I := Result to FCount - 1 do
    case FList^[I].FKeyType of
      ikIdent,
      ikEmptyIdent: Result := Succ(I);
      ikSection: Break;
    end;
  Insert(Result, Trim(Ident));

  with FList^[Result] do
  begin
    FKeyType := ikIdent;
    FIdentHash := FHashOfFunc(FIdentStr);
  end;

  // No need to update the indexes when the new item is last in the array.
  if Result < FCount - 1 then
    FSections.UpdateIndexes(Result, +1);

  if FHashAll then
    FSections.AddIniItem(P, @FList^[Result]);
end;

function TIniItems.AppendSection(const Section: string): PSectionItem;
var
  I: Integer;
begin
  if (FCount > 0) and (FList^[FCount - 1].FKeyType <> ikBlank) then
    Add('');
  I := Add('[' + Section + ']');
  Result := FSections.Find(Section)^;
  if Result = nil then
    Result := FSections.Add(Section, Succ(I)); // This should never happen!
end;

procedure TIniItems.Clear;
begin
  if FCount <> 0 then
  begin
    Finalize(FList^[0], FCount);
    FCount := 0;
    SetCapacity(0);
  end;
  FSections.Clear;
  FStatistics.Clear;
  FLastSectionItem := nil;
end;

procedure TIniItems.Delete(Index: Integer);
begin
  Delete(Index, 1);
end;

procedure TIniItems.Delete(Index, N: Integer);
var
  I: Integer;
begin
  if (Index < 0) or (Index + N > FCount) then Error(@RTLConsts.SListIndexError, Index);

  for I := 0 to N - 1 do
    case FList^[Index+I].FKeyType of
      ikBlank:   Dec(FStatistics.FBlankLines);
      ikComment: Dec(FStatistics.FComments);
      ikSection: Dec(FStatistics.FSections);
    else         Dec(FStatistics.FIdents);
    end;

  Finalize(FList^[Index], N);
  Dec(FCount, N);
  if Index < FCount then
    System.Move(FList^[Index + N], FList^[Index],
      (FCount - Index) * SizeOf(TIniItem));
end;

procedure TIniItems.EraseSection(const Section: string;
  RemoveLeadingCommentLines: Boolean; const OnlyItems : Boolean);
var
  I, L, B: Integer;
  Index: Integer;
begin
  Index := IndexOfSection(Section, False);
  if Index <> -1 then
  begin
    Dec(Index); // Let it point to 'self' = [Section] for later use.
    L := Index;
    B := 0; // Counts number of blank lines (if too 'much' remove them as well)
    // First, find last TIniItem belonging to this section.
    for I := Succ(Index) to FCount - 1 do
      case FList^[I].FKeyType of
        ikSection:
          Break;

        ikIdent,
        ikRawIdent,
        ikEmptyIdent:
          begin
            B := 0;
            L := I; // Remember last to be removed.
          end;

        // Try to keep comment of next Section. When there are to much
        // blank lines (>1) between the comment and the next section it
        // will still be removed.
        ikComment:
           B := -1;

        ikBlank:
          begin
            Inc(B);
            if B > 0 then
              L := I; // Remember last to be removed.
          end;
      end;

    if not OnlyItems then
      if RemoveLeadingCommentLines then
        for I := Pred(Index) downto 0 do
          if FList^[I].FKeyType = ikComment then
            Dec(Index)
          else
            Break;

    // Now really delete all the stuff.
    if L >= Index then
    begin
      if OnlyItems then
      begin
        Inc(Index);

        // To keep at least one blank line between sections
        if FList^[L].FKeyType = ikBlank then
          Dec(L);
      end;

      Delete(Index, Succ(L - Index));
    end;


    if FLastSectionItem <> nil then
      if FCompareFunc(Section, FLastSectionItem.FName) then
        FLastSectionItem := nil;

    if not OnlyItems then
      FSections.Delete(Section);

    FSections.UpdateIndexes(Index, Index - L - 1);
  end;
end;

procedure TIniItems.Error(const Msg: string; Data: Integer);
{$IFDEF FPC}
begin
  raise EStringListError.CreateFmt(Msg, [Data]) at get_caller_addr(get_frame);
end;
{$ELSE}
  function ReturnAddr: Pointer;
  asm
        mov   eax,[ebp+4]
  end;
begin
  raise EStringListError.CreateFmt(Msg, [Data]) at ReturnAddr;
end;
{$ENDIF}

procedure TIniItems.Error(Msg: PResStringRec; Data: Integer);
begin
  Error(LoadResString(Msg), Data);
end;

function TIniItems.FindIniItem(const Section, Ident: string;
  Force: Boolean): PIniItem;
var
  L: PtrUInt;
  PI: PPIniItems;
  PS: PSectionItem;
begin
  Result := nil;

  PS := FindSectionItem(Section, Force);
  if PS <> nil then
  begin
    if FDirtyHash then
    begin
      FSections.FreeAndNilAllIdentBuckets;
      FDirtyHash := False;
    end;

    if PS.FIdentBuckets = nil then
      HashIdents(PS);

    PI := PS.FIdentBuckets[FHashOfFunc(Ident) mod MAXBUCKETS];

    {$IFDEF FPC}{$IFDEF ENDIAN_LITTLE} {$HINTS OFF} {$ENDIF}{$ENDIF}
      // Note: P^[0] is used as a "Cardinal" to hold the count of items.
      if PI <> nil then
        for L := PtrUInt(PI^[0]) downto 1 do
          if FCompareFunc(Ident, PI^[L].FIdentStr) then
          begin
            Result := PI^[L];
            Exit;
          end;
    {$IFDEF FPC}{$IFDEF ENDIAN_LITTLE} {$HINTS ON} {$ENDIF}{$ENDIF}
  end;

  if (Result = nil) and Force then
    Result := Items[AppendIdent(Section, Ident)];
end;

function TIniItems.FindSectionItem(const Section: string;
  Force: Boolean): PSectionItem;
begin
  if (Section <> '') and (FLastSectionItem <> nil) then
    if FCompareFunc(Section, FLastSectionItem.FName) then
    begin
      Result := FLastSectionItem;
      Exit;
    end;

  Result := FSections.Find(Section)^;
  if Force and (Result = nil) then
    Result := AppendSection(Section);

  if Result <> nil then
    FLastSectionItem := Result;
end;

function TIniItems.Get(Index: Integer): string;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  with FList^[Index] do
    if FKeyType = ikIdent then
      Result := FIdentStr + '=' + FValueStr
    else
      Result := FIdentStr;
end;

function TIniItems.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TIniItems.GetCount: Integer;
begin
  Result := FCount;
end;

// Note: When this function is modified also check IndexOfIdent()
function TIniItems.GetItem(Index: Integer): PIniItem;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  with FList^[Index] do
    if FKeyType = ikRawIdent then
    begin
      if SplitStrings(FIdentStr, FValueStr) then
      begin
        FKeyType := ikIdent;
        FIdentHash := FHashOfFunc(FIdentStr);
      end
      else
        FKeyType := ikEmptyIdent;
    end;
  Result := @FList^[Index];
end;

function TIniItems.GetName(Index: Integer): string;
begin
  Result := Items[Index].FIdentStr;
end;

function TIniItems.GetTextStr: string;
var
  I, L, Size, Count: Integer;
  P: PChar;
  S: string;
begin
  Count := GetCount;
  Size := 0;
{$IFDEF LINUX}
  for I := 0 to Count - 1 do Inc(Size, Length(Get(I)) + 1);
{$ELSE}
  for I := 0 to Count - 1 do Inc(Size, Length(Get(I)) + 2);
{$ENDIF}
  SetString(Result, nil, Size);
  P := Pointer(Result);
  for I := 0 to Count - 1 do
  begin
    S := Get(I);
    L := Length(S);
    if L <> 0 then
    begin
      System.Move(Pointer(S)^, P^, L * SizeOf(Char));
      Inc(P, L);
    end;
{$IFNDEF LINUX}
    P^ := #13;
    Inc(P);
{$ENDIF}
    P^ := #10;
    Inc(P);
  end;
end;

function TIniItems.GetValue(Index: Integer): string;
begin
  Result := Items[Index].FValueStr;
// << 20021014 Following assumption did not work on newly created INI-files
//  if (Index < 0) or (Index > FCount) then Error(@SListIndexError, Index);
//  Result := FList^[Index].ValueStr; // Assume RawIdents already translated.
// >>
end;

procedure TIniItems.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 1024 then Delta := FCapacity div 4 else
    if FCapacity > 256 then Delta := 512 else
      Delta := 128;
  SetCapacity(FCapacity + Delta);
end;

procedure TIniItems.HashIdents(PS: PSectionItem);
var
  I: Integer;
begin
  if PS.FIdentBuckets = nil then
  begin
    New(PS.FIdentBuckets);
    with PS^ do
      for I := 0 to MAXBUCKETS - 1 do
        FIdentBuckets[I] := nil;
  end
  else
    FSections.ClearIdentBuckets(PS.FIdentBuckets);

  for I := PS.FIndex to FCount - 1 do
    case Items[I].FKeyType of
      ikIdent:   FSections.AddIniItem(PS, @FList^[I]);
      ikSection: Break;
    end;
end;

// --- Returns the index of the specified Ident.
function TIniItems.IndexOfIdent(const Section, Ident: string;
  Force: Boolean): Integer;
var
  Hash: Cardinal;
  I: Integer;
begin
  Result := IndexOfSection(Section, Force);
  if Result <> -1 then
  begin
    I := Result;
    Result := -1;
    Hash   := FHashOfFunc(Ident);
    for I := I to FCount - 1 do
      with FList^[I] {GetItem(I)^} do
      begin
        case FKeyType of
          ikIdent:
            ;       // Note: this will execute the hash comparison (below)

          ikRawIdent:
            // << Dirty coding style (this code is the same as in GetItem).
            //    Excuse... it saves a lot of time on large sections.
            if SplitStrings(FIdentStr, FValueStr) then
            begin
              FKeyType := ikIdent;
              FIdentHash := FHashOfFunc(FIdentStr);
              // Note: this will execute the hash comparison (below)
            end
            else
            begin
              FKeyType := ikEmptyIdent;
              Continue;
            end;
            // >>

          ikSection:
            Break;   // Ok, hit the brakes, we've ended up at the next section
        else
          Continue;  // Oh well, lets try another.
        end;

        if FIdentHash = Hash then
          if FCompareFunc(Ident, FIdentStr) then
          begin
            Result := I;
            Exit;
          end;
      end;
  end;
  if (Result = -1) and Force then
    Result := AppendIdent(Section, Ident);
end;

// --- Returns the index of the first item of the specified Section.
function TIniItems.IndexOfSection(const Section: string;
  Force: Boolean): Integer;
var
  P: PSectionItem;
begin
  P := FindSectionItem(Section, Force);
  if P <> nil then
    Result := P^.FIndex
  else
    Result := -1;
end;

procedure TIniItems.Insert(Index: Integer; const S: string);
begin
  if (Index < 0) or (Index > FCount) then Error(@SListIndexError, Index);
  InsertItem(Index, S);
end;

procedure TIniItems.InsertItem(Index: Integer; const S: string);
begin
  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(TIniItem));
  with FList^[Index] do
  begin
    Pointer(FIdentStr) := nil;
    Pointer(FValueStr) := nil;
    if S = '' then
    begin
      FKeyType := ikBlank;
      Inc(FStatistics.FBlankLines);
    end else
    if S[1] = ';' then
    begin
      FKeyType  := ikComment;
      FIdentStr := S;
      Inc(FStatistics.FComments);
    end else
    if (S[1] = '[') and (S[Length(S)] = ']') then
    begin
      FKeyType  := ikSection;
      FIdentStr := S;
      FSections.Add(Copy(S, 2, Length(S) - 2), Succ(FCount));
      Inc(FStatistics.FSections);
    end else
    begin
      FKeyType  := ikRawIdent;
      FIdentStr := S;
      Inc(FStatistics.FIdents);
    end;
  end;
  Inc(FCount);
end;

procedure TIniItems.LoadFromFile(const AFileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

{$IFDEF COMPILER12_UP}
procedure TIniItems.LoadFromFile(const AFileName: string; Encoding: TEncoding);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream, Encoding);
  finally
    Stream.Free;
  end;
end;
{$ENDIF}

{$IFDEF COMPILER12_UP}
procedure TIniItems.LoadFromStream(Stream: TStream);
begin
  LoadFromStream(Stream, nil);
end;

procedure TIniItems.LoadFromStream(Stream: TStream; Encoding: TEncoding);
var
  Size: Integer;
  Buffer: TBytes;
begin
  Size := Stream.Size - Stream.Position;
  SetLength(Buffer, Size);
  Stream.Read(Buffer[0], Size);

  Size := TEncoding.GetBufferEncoding(Buffer, Encoding);
  SetTextStr(Encoding.GetString(Buffer, Size, Length(Buffer) - Size));
end;

{$ELSE}

procedure TIniItems.LoadFromStream(Stream: TStream);
var
  Size: Integer;
  S: string;
begin
  Size := Stream.Size - Stream.Position;
  SetString(S, nil, Size);
  Stream.Read(Pointer(S)^, Size);
  SetTextStr(S);
end;
{$ENDIF}

procedure TIniItems.Put(Index: Integer; const S: string);
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  FList^[Index].FIdentStr := S;
end;

function TIniItems.RenameSection(const OldName, NewName: string): Boolean;
var
  PPS: PPSectionItem;
  Index: Integer;
begin
  Result := False;

  // Check if the <NewName> exists, if so, fail.
  PPS := FSections.Find(NewName);
  if PPS^ = nil then
  begin
    // Check if the <OldName> exists, if not, fail.
    PPS := FSections.Find(OldName);
    if PPS^ <> nil then
    begin
      if FLastSectionItem <> nil then
        if FCompareFunc(OldName, FLastSectionItem.FName) then
          FLastSectionItem := nil;

      // Get the index in IniItems list and let it point to "self" (= [Section]).
      Index := Pred(PPS^.FIndex);

      // "Rename" section in IniItems list.
      FList^[Index].FIdentStr := '[' + NewName + ']';

      // Delete old one from sections and ...
      FSections.Delete(PPS);
      // ... add new one, the IdentBuckets will be re-hashed when needed.
      FSections.Add(NewName, Succ(Index));

      Result := True;
    end;
  end;
end;

{$IFDEF COMPILER12_UP}
procedure TIniItems.SaveToFile(const AFileName: string);
begin
  SaveToFile(AFileName, nil);
end;

procedure TIniItems.SaveToFile(const AFileName: string; Encoding: TEncoding);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(Stream, Encoding);
  finally
    Stream.Free;
  end;
end;

{$ELSE}

procedure TIniItems.SaveToFile(const AFileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;
{$ENDIF}

{$IFDEF COMPILER12_UP}
procedure TIniItems.SaveToStream(Stream: TStream);
begin
  SaveToStream(Stream, nil);
end;

procedure TIniItems.SaveToStream(Stream: TStream; Encoding: TEncoding);
var
  Buffer, Preamble: TBytes;
begin
  if Encoding = nil then
    Encoding := TEncoding.Default;
  Buffer := Encoding.GetBytes(GetTextStr);
  Preamble := Encoding.GetPreamble;
  if Length(Preamble) > 0 then
    Stream.WriteBuffer(Preamble[0], Length(Preamble));
  Stream.WriteBuffer(Buffer[0], Length(Buffer));
end;

{$ELSE}

procedure TIniItems.SaveToStream(Stream: TStream);
var
  S: string;
begin
  S := GetTextStr;
  Stream.WriteBuffer(Pointer(S)^, Length(S));
  if FFlushFileBuffers and (Stream is TFileStream) then
    Windows.FlushFileBuffers(TFileStream(Stream).Handle);
end;
{$ENDIF}

procedure TIniItems.SetCapacity(NewCapacity: Integer);
var
  P: pointer;
begin
  P := FList;
  ReallocMem(FList, NewCapacity * SizeOf(TIniItem));
  if FList <> P then
    FDirtyHash := True;
  FCapacity := NewCapacity;
end;

procedure TIniItems.SetCompareFunc(const Value: TCompareFunc);
begin
  FCompareFunc := Value;
  FSections.FCompareFunc := Value;
end;

procedure TIniItems.SetHashAll(const Value: Boolean);
begin
  if FHashAll <> Value then
  begin
    FHashAll := Value;
    FSections.FHashAll := Value;
  end;
end;

procedure TIniItems.SetHashOfFunc(const Value: THashOfFunc);
begin
  FHashOfFunc := Value;
  FSections.FHashOfFunc := Value;
end;

procedure TIniItems.SetTextStr(const Value: string);
var
  S: string;
  P, Start: PChar;
begin
  Clear;
  Capacity := Length(Value) div 16;     // estimate 16 characters per item.

  P := Pointer(Value);
  if P <> nil then
    while P^ <> #0 do
    begin
      Start := P;
{$IFDEF COMPILER12_UP} {$WARN WIDECHAR_REDUCED OFF} {$ENDIF}
      while not (P^ in [#0, #10, #13]) do Inc(P);
{$IFDEF COMPILER12_UP} {$WARN WIDECHAR_REDUCED ON} {$ENDIF}
      SetString(S, Start, P - Start);
      Add(S);
      if P^ = #13 then Inc(P);
      if P^ = #10 then Inc(P);
    end;

  Capacity := FCount;

  if FStatistics.FSections > 0 then
    if (FStatistics.FIdents div FStatistics.FSections) >= 250 then
      HashAll := True;
end;

procedure TIniItems.SetValue(Index: Integer; const S: string);
begin
  if (Index < 0) or (Index > FCount) then Error(@SListIndexError, Index);
  FList^[Index].FValueStr := S;
end;

// --- Split the incoming (raw) Ident into a Ident and Value string.
// Note: only to be used by/with GetItem().
{$IFDEF COMPILER12_UP} {$WARN WIDECHAR_REDUCED OFF} {$ENDIF}
function TIniItems.SplitStrings(var Ident, Value: string): Boolean;
var
  I1, I2: Integer;
  V1, V2: Integer;
  C, S, P: PChar;
begin
  Result := False;
  if not Assigned(Pointer(Ident)) then Exit;
  P := Pointer(Ident);
  S := P;
  // Find last of the leading spaces of the "Ident".
  while (P^ in [#1..' ']) do Inc(P);
  I1 := P - S;
  // Find equal sign.
  while not (P^ in [#0, '=']) do Inc(P);
  I2 := P - S;

  // If there is an equal sign remove trailing spaces from the identifier
  // and get the stuff behind it.
  if P^ = '=' then
  begin
    Result := True;
    // Find and 'remove' possible trailing spaces from "Ident".
    if (P - S) > 1 then
    begin
      C := P; // Copy pointer
      repeat
        Dec(P);
      until not (P^ in [#1..' ']) or (P <= S);
      I2 := P - S + 1;
      P := C; // Restore pointer
    end;

    Inc(P);
    // Find last of the leading spaces of "Value"
    while (P^ in [#1..' ']) do Inc(P);
    V1 := P - S;
    // Find trailing spaces of the "Value".
    P := S + Length(Ident);
    while P^ <= ' ' do Dec(P);
    V2 := P - S + 1;
    // Move "Value" from Ident string to Value string.
    if V2 > V1 then
    begin
      SetLength(Value, V2 - V1);
      Move((S + V1)^, Pointer(Value)^, (V2 - V1) * SizeOf(Char));
    end;
  end;

  // Shrink Ident so that it only contains the found Identifier.
  if I2 > I1 then
  begin
    Move((S + I1)^, S^, (I2 - I1) * SizeOf(Char));
    SetLength(Ident, I2 - I1);
  end;
end;
{$IFDEF COMPILER12_UP} {$WARN WIDECHAR_REDUCED ON} {$ENDIF}


{ - TFastIniFile - }

{$IFDEF COMPILER12_UP}
constructor TFastIniFile.Create(const AFileName: string);
begin
  Create(AFileName, nil);
end;

constructor TFastIniFile.Create(const AFileName: string; Encoding: TEncoding);
begin
  inherited Create(AFileName);
  FEncoding := Encoding;
  FIniItems := TIniItems.Create;
  InlineCommentsEnabled := False;
  PasswordSeed := PASSWORD_SEED;
  PasswordMod := PASSWORD_MOD;
{$IFDEF LINUX}
  CaseSensitive := True;
{$ELSE}
  UseAnsiCompare := True;
{$ENDIF}
  FStripQuotes := True;
end;

{$ELSE}

constructor TFastIniFile.Create(const AFileName: string);
begin
  inherited Create(AFileName);
  FIniItems := TIniItems.Create;
  InlineCommentsEnabled := True;
  PasswordSeed := PASSWORD_SEED;
  PasswordMod := PASSWORD_MOD;
{$IFDEF LINUX}
  CaseSensitive := True;
{$ELSE}
  UseAnsiCompare := True;
{$ENDIF}
  FStripQuotes := True;
end;
{$ENDIF}

destructor TFastIniFile.Destroy;
begin
  try
    FlushBuffers;
  finally
    Clear;
    FIniItems.Free;
  end;
end;

function TFastIniFile.AppendSection(const Section: string): Boolean;
begin
  Result := False;
  if not SectionExists(Section) then
  begin
    Result := FIniItems.AppendSection(Section) <> nil;
    if Result then
      FModified := True;
  end;
end;

procedure TFastIniFile.Clear;
begin
  FIniItems.Clear;
  FLoaded := False;
  FModified := False;
end;

procedure TFastIniFile.ClearSection(const Section: string);
begin
  if SectionExists(Section) then
  begin
    FIniItems.EraseSection(Section, False, True);
    FModified := True;
  end;
end;

function TFastIniFile.CompareStrings(S1, S2: TStrings): Integer;
var
  I: Integer;
begin
  Result := 0;
  if S1.Count > S2.Count then
    Result := 1
  else
    if S1.Count < S2.Count then
      Result := -1
    else
      for I := 0 to S1.Count - 1 do
      begin
        Result := CompareStr(S1[I], S2[I]);
        if Result <> 0 then
          Exit;
      end;
end;

procedure TFastIniFile.DeleteKey(const Section, Ident: string);
var
  Index: Integer;
  P: PSectionItem;
begin
  if not FLoaded then LoadValues;
  Index := FIniItems.IndexOfIdent(Section, Ident, False);
  if Index <> -1 then
  begin
    FIniItems.Delete(Index);
    FIniItems.Sections.UpdateIndexes(Index, -1);
    // Throw away the FIdentBuckets they are no longer valid.
    if HashAll then
    begin
      P := FIniItems.FindSectionItem(Section, False);
      FIniItems.FSections.FreeAndNilIdentBuckets(P.FIdentBuckets);
    end;
    FModified := True;
  end;
end;

procedure TFastIniFile.EraseSection(const Section: string);
begin
  EraseSection(Section, False);
end;

procedure TFastIniFile.EraseSection(const Section: string;
  RemoveLeadingCommentLines: Boolean);
begin
  if not FLoaded then LoadValues;

  FIniItems.EraseSection(Section, RemoveLeadingCommentLines, False);
  FModified := True;
end;

procedure TFastIniFile.FlushBuffers;
begin
  if FModified then
  begin
{$IFDEF COMPILER12_UP}
    FIniItems.SaveToFile(FileName, FEncoding);
{$ELSE}
    FIniItems.SaveToFile(FileName);
{$ENDIF}
  end;
  FModified := False;
end;

function TFastIniFile.GetCount: Integer;
begin
  if not FLoaded then LoadValues;
  Result := FIniItems.Count;
end;

function TFastIniFile.GetFlushFileBuffers: Boolean;
begin
  Result := FIniItems.FlushFileBuffers;
end;

function TFastIniFile.GetHashAll: Boolean;
begin
  Result := FIniItems.HashAll;
end;

function TFastIniFile.GetStatistics: TStatistics;
begin
  if not FLoaded then LoadValues;
  Result := FIniItems.Statistics;
end;

procedure TFastIniFile.GetStrings(List: TStrings);
var
  I: Integer;
begin
  if not FLoaded then LoadValues;
  List.BeginUpdate;
  try
    for I := 0 to FIniItems.Count - 1 do
      List.Add(FIniItems[I]);
  finally
    List.EndUpdate;
  end;
end;

function TFastIniFile.IncludeLeadingSemicolon(const Comment: string): string;
begin
  Result := Comment;
  // Only allow blank lines when FAllowBlankLine is True
  if not (FAllowBlankLine and (Comment = '')) then
    if (Comment = '') or (Comment[1] <> ';') then Result := ';' + Result;
end;

function TFastIniFile.InlineComment(const Section, Ident, Comment: string;
  Where: Integer; Mode: TInsertModes): Boolean;
var
  CommentPos: Integer;
  Index: Integer;
  L: Integer;
  S: string;
begin
  Result := False;
  if not FLoaded then LoadValues;

  Index := FIniItems.IndexOfIdent(Section, Ident, imCanCreateKey in Mode);
  if Index <> -1 then
  begin
    S := FIniItems.Values[Index];

    // See if there is already a comment.
    CommentPos := FindFirstComment(S);
    if CommentPos > 0 then
    begin
      // Bail out when it's not allowed to overwrite existing comment.
      if not (imOverwriteExisting in Mode) then Exit;

      // Remove the old comment.
      S := Copy(S, 1, CommentPos - 1);
    end;

    S := Trim(S);
    L := Length(Trim(Ident)) + Length(S) + 2;
    if L >= Where then
      FIniItems.Values[Index] := S + ' ' + IncludeLeadingSemicolon(Comment)
    else
      FIniItems.Values[Index] := S + StringOfChar(' ', Where - L) +
       IncludeLeadingSemicolon(Comment);

    FModified := True;
    Result := True;
  end;
end;

function TFastIniFile.InsertComment(const Section, Ident, Comment: string;
  Where: TInsertPosition; Mode: TInsertModes): Boolean;
var
  Index: Integer;
  Inserted: Boolean;

  procedure LocalInsert;
  begin
    FIniItems.Insert(Index, IncludeLeadingSemicolon(Comment));
    Inserted := True;
  end;

  procedure LocalOverwrite(I: Integer);
  begin
    if (I >= 0) and (I < FIniItems.Count) and
      (FIniItems.Items[I].FKeyType = ikComment) then
    begin
      FIniItems[I] := IncludeLeadingSemicolon(Comment);
    end
    else
      LocalInsert;
  end;

begin
  if not FLoaded then LoadValues;

  Inserted := False;
  case Where of
    ipBeforeSection,
    ipAfterSection:
      begin
        Index := FIniItems.IndexOfSection(Section, imCanCreateKey in Mode);
        if Index <> -1 then
        begin
          if Where = ipBeforeSection then Dec(Index);
          if (imOverwriteExisting in Mode) then
          begin
            if Where = ipBeforeSection then
              LocalOverwrite(Index - 1)
            else
              LocalOverwrite(Index);
          end
          else
            LocalInsert;
          if Where = ipAfterSection then Dec(Index);
          FModified := True;
        end;
      end;

    ipBeforeIdent,
    ipAfterIdent:
      begin
        Index := FIniItems.IndexOfIdent(Section, Ident, imCanCreateKey in Mode);
        if Index <> -1 then
        begin
          if Where = ipAfterIdent then Inc(Index);
          if (imOverwriteExisting in Mode) then
          begin
            if Where = ipBeforeIdent then
              LocalOverwrite(Index - 1)
            else
              LocalOverwrite(Index);
          end
          else
            LocalInsert;
          if Where = ipBeforeIdent then Inc(Index);
          FModified := True;
        end;
      end;
  else // ipAppend
    Index := FIniItems.Add(IncludeLeadingSemicolon(Comment));
    FModified := True;
  end;

  if Inserted then
    FIniItems.Sections.UpdateIndexes(Index, +1);
  Result := Index <> -1;
end;

function TFastIniFile.InsertBlankLine(const Section, Ident : string; Where: TInsertPosition): Boolean;
begin
  FAllowBlankLine := True;
  try
    Result := InsertComment(Section, Ident, '', Where, []);
  finally
    FAllowBlankLine := False;
  end;
end;

procedure TFastIniFile.LoadValues;
begin
  if (FileName <> '') and FileExists(FileName) then
  begin
{$IFDEF COMPILER12_UP}
    FIniItems.LoadFromFile(FileName, FEncoding);
{$ELSE}
    FIniItems.LoadFromFile(FileName);
{$ENDIF}
  end
  else
    Clear;
  FLoaded := True; // Assume loaded even when file doesn't exists yet.
end;

function TFastIniFile.ReadInlineComment(const Section, Ident,
  Default: string): string;
var
  CommentPos: Integer;
  Index: Integer;
begin
  Result := Default;
  if not FLoaded then LoadValues;

  Index := FIniItems.IndexOfIdent(Section, Ident, False);
  if Index <> -1 then
  begin
    Result := FIniItems.Values[Index];

    CommentPos := FindFirstComment(Result);
    if CommentPos > 0 then
      Result := Copy(Result, CommentPos + 1, 255)
    else
      Result := Default;
  end;
end;

procedure TFastIniFile.ReadSection(const Section: string; Strings: TStrings);
var
  I: Integer;
begin
  if not FLoaded then LoadValues;
  Strings.BeginUpdate;
  try
    Strings.Clear;
    I := FIniItems.IndexOfSection(Section, False);
    if I <> -1 then
      for I := I to FIniItems.Count - 1 do
        with FIniItems.Items[I]^ do
          case FKeyType of
            ikIdent:   Strings.Add(FIdentStr);
            ikSection: Break;
          end;
  finally
    Strings.EndUpdate;
  end;
end;

procedure TFastIniFile.ReadSections(Strings: TStrings);
var
  I: Integer;
begin
  if not FLoaded then LoadValues;
  Strings.BeginUpdate;
  try
    Strings.Clear;
    for I := 0 to FIniItems.Count - 1 do
      with FIniItems.Items[I]^ do
        if FKeyType = ikSection then
          Strings.Add(Copy(FIdentStr, 2, Length(FIdentStr) - 2));
  finally
    Strings.EndUpdate;
  end;
end;

procedure TFastIniFile.ReadSectionValues(const Section: string; Strings: TStrings);
var
  I: Integer;
begin
  if not FLoaded then LoadValues;
  Strings.BeginUpdate;
  try
    Strings.Clear;
    I := FIniItems.IndexOfSection(Section, False);
    if I <> -1 then
      for I := I to FIniItems.Count - 1 do
        with FIniItems.Items[I]^ do
          case FKeyType of
            ikIdent:
              Strings.Add(FIdentStr + '=' + ReadString(Section, FIdentStr, ''));
            ikSection:
              Break;
          end;
  finally
    Strings.EndUpdate;
  end;
end;

// --- This TFastIniFile is rather pointer hungry. The next function
//     enables you to safely throw away the pointers without destroying this
//     object. Use with care, everything needs to be reread when you access
//     one of the sections/idents/values again.
procedure TFastIniFile.Release;
begin
  FlushBuffers;
  Clear;
end;

procedure TFastIniFile.Rename(const AFileName: string; Reload: Boolean);
begin
  inherited Create(AFileName); // Yikes :)  just to get FFileName correct
  if Reload then LoadValues;
end;

function TFastIniFile.RenameSection(const OldName, NewName: string): Boolean;
begin
  if not FLoaded then LoadValues;
  Result := FIniItems.RenameSection(OldName, NewName);
  if Result then
    FModified := True;
end;

function TFastIniFile.SectionExists(const Section: string): Boolean;
begin
  if not FLoaded then LoadValues;
  Result := FIniItems.IndexOfSection(Section, False) <> -1;
end;

procedure TFastIniFile.SetCaseSensitive(const Value: Boolean);
begin
  FCaseSensitive := Value;
  FIniItems.FSections.FCaseSensitive := Value;
  UseAnsiCompare := UseAnsiCompare;
end;

procedure TFastIniFile.SetFlushFileBuffers(const Value: Boolean);
begin
  FIniItems.FlushFileBuffers := Value;
end;

procedure TFastIniFile.SetHashAll(const Value: Boolean);
begin
  FIniItems.HashAll := Value;
end;

procedure TFastIniFile.SetStrings(List: TStrings; ForceUpdate: Boolean);
var
  SL: TStringList;
begin
  if FModified or ForceUpdate then
    FModified := True
  else
  begin
    // Check if the incoming list is the same as the current ini-contents.
    SL := TStringList.Create;
    try
      GetStrings(SL);
      FModified := CompareStrings(SL, List) <> 0;
    finally
      SL.Free;
    end;
  end;

  if FModified then
  begin
    FIniItems.SetTextStr(List.Text);
    FLoaded := True;
  end;
end;

procedure TFastIniFile.SetUseAnsiCompare(Value: Boolean);
begin
  FUseAnsiCompare := Value;
  if FCaseSensitive then
  begin
    FIniItems.CompareFunc := SameStr;
    FIniItems.HashOfFunc := HashOfStr;
  end
  else
    if FUseAnsiCompare then
    begin
      FIniItems.CompareFunc := AnsiSameText;
      FIniItems.HashOfFunc := HashOfAnsi;
    end
    else
    begin
      FIniItems.CompareFunc := SameText;
      FIniItems.HashOfFunc := HashOfText;
    end;
end;

procedure TFastIniFile.UpdateFile;
begin
  FlushBuffers;
end;

function TFastIniFile.ValueExist(const Section, Ident: string): Boolean;
begin
  if not FLoaded then LoadValues;
  Result := FIniItems.IndexOfIdent(Section, Ident, False) <> -1;
end;


function TFastIniFile.ReadString(const Section, Ident, Default: string): string;
var
  CommentPos: Integer;
  Index, L: Integer;
  P: PIniItem;
begin
  Result := Default;
  if not FLoaded then LoadValues;

  if FIniItems.FHashAll then
  begin
    P := FIniItems.FindIniItem(Section, Ident, False);
    if P = nil then Exit;
    Result := P.FValueStr;
  end
  else
  begin
    Index := FIniItems.IndexOfIdent(Section, Ident, False);
    if Index = -1 then Exit;
    Result := FIniItems.Values[Index];
  end;

  // Check if it has inline comment. If so, remove it.
  if FInlineCommentsEnabled then
  begin
    CommentPos := FindFirstComment(Result);
    if CommentPos > 0 then
      Result := Trim(Copy(Result, 1, CommentPos - 1));
  end;

  // Remove quotes from Value
  if FStripQuotes then
  begin
{$IFDEF COMPILER12_UP} {$WARN WIDECHAR_REDUCED OFF} {$ENDIF}
    L := Length(Result);
    if (L >= 2) and (Result[1] in ['''', '"']) and (Result[1] = Result[L]) then
      Result := Copy(Result, 2, L - 2);
{$IFDEF COMPILER12_UP} {$WARN WIDECHAR_REDUCED ON} {$ENDIF}
  end;
end;

procedure TFastIniFile.WriteString(const Section, Ident, Value: string);
var
  Comment: string;
  CommentPos: Integer;
  L: Integer;
  P: PIniItem;
  S: string;
begin
  if not FLoaded then LoadValues;

  if Ident = '' then
  begin
    // Simulate WritePrivateProfileString behaviour:
    // - Only Value is empty string        -> Sorry, can't handle it here,
    //   because somebody might actually want to write a "Ident=" in the file.
    // - all parameters are empty strings  -> Flush file to disk
    // - Ident and Value are empty strings -> Erase section
    if Value = '' then
      if Section = '' then
        FlushBuffers
      else
        EraseSection(Section, False);
  end
  else
  begin
    if FIniItems.FHashAll then
      P := FIniItems.FindIniItem(Section, Ident, True)
    else
      P := FIniItems.Items[FIniItems.IndexOfIdent(Section, Ident, True)];

    S := P.FValueStr;

    // Check if the old string has an inline comment. If so, save it and remove
    // it from the old string before comparing it against the new string.
    CommentPos := 0;
    if FInlineCommentsEnabled then
    begin
      CommentPos := FindFirstComment(S);
      if CommentPos > 0 then
      begin
        Comment := Copy(S, CommentPos, 255);
        S := Trim(Copy(S, 1, CommentPos - 1));
      end;
    end;

    if S <> Value then
    begin
      // It there was a comment, glue it back on and
      // try to keep the comment on its original position.
      if CommentPos > 0 then
      begin
        L := Length(Value) + 1;
        if L >= CommentPos then
          P.FValueStr := Value + ' ' + Comment
        else
          P.FValueStr := Value + StringOfChar(' ', CommentPos - L) + Comment;
      end
      else
        P.FValueStr := Value;
      FModified := True;
    end;
  end;
end;



function TFastIniFile.ReadBoolean(const Section, Ident: string;
  Default: Boolean): Boolean;
begin
  Result := ReadBool(Section, Ident, Default);
end;

procedure TFastIniFile.WriteBoolean(const Section, Ident: string; Value: Boolean);
begin
  WriteBool(Section, Ident, Value);
end;

{$IFDEF UseVCL}
function TFastIniFile.ReadFont(const Section, Ident: string;
  const Default: TFont): TFont;
begin
  Result := StrToFont(ReadString(Section, Ident, FontToStr(Default)), Default);
end;

procedure TFastIniFile.WriteFont(const Section, Ident: string;
  const Value: TFont);
begin
  WriteString(Section, Ident, FontToStr(Value));
end;
{$ENDIF}

procedure TFastIniFile.WriteIntHex2(const Section, Ident: string; Value: LongInt);
begin
  WriteString(Section, Ident, Format('$%.2X', [Word(Value)]));
end;

procedure TFastIniFile.WriteIntHex3(const Section, Ident: string; Value: LongInt);
begin
  WriteString(Section, Ident, Format('$%.3X', [Word(Value)]));
end;

procedure TFastIniFile.WriteIntHex4(const Section, Ident: string; Value: LongInt);
begin
  WriteString(Section, Ident, Format('$%.4X', [Word(Value)]));
end;

procedure TFastIniFile.WriteIntHex8(const Section, Ident: string; Value: LongInt);
begin
  WriteString(Section, Ident, Format('$%.8X', [Value]));
end;

function TFastIniFile.ReadReal(const Section, Ident: string;
  Default: Extended): Extended;
var
  TempStr: string;
  ResFloat: Extended;
  Error: Integer;
begin
  Result  := Default;
  TempStr := ReadString(Section, Ident, '?');
  if TempStr <> '?' then
  begin
    Val(TempStr, ResFloat, Error);
    if Error = 0 then
      Result := ResFloat;
  end;
end;

procedure TFastIniFile.WriteReal(const Section, Ident: string; Value: Extended);
begin
  WriteString(Section, Ident, FloatToStrF(Value, ffGeneral, 9, 0));
end;

procedure TFastIniFile.WriteRealF(const Section, Ident: string;
  Value: Extended; MinDecimals: Integer);
var
  TmpPos: Integer;
  TmpStr: string;
begin
  TmpStr := FloatToStrF(Value, ffGeneral, 9, 0);
  if MinDecimals > 0 then
  begin
    TmpPos := Pos('.', TmpStr);
    if TmpPos = 0 then
    begin
      TmpStr := TmpStr + '.';
      TmpPos := Pos('.', TmpStr);
    end;
    while (Length(TmpStr) - MinDecimals) < TmpPos do TmpStr := TmpStr + '0';
  end;
  WriteString(Section, Ident, TmpStr);
end;

function TFastIniFile.ReadPoint(const Section, Ident: string;
  const Default: TPoint): TPoint;
begin
  Result := StrToPoint(ReadString(Section, Ident, PointToStr(Default)), Default);
end;

procedure TFastIniFile.WritePoint(const Section, Ident: string;
  const Value: TPoint);
begin
  WriteString(Section, Ident, PointToStr(Value));
end;

function TFastIniFile.ReadRect(const Section, Ident: string;
  const Default: TRect): TRect;
begin
  Result := StrToRect(ReadString(Section, Ident, RectToStr(Default)), Default);
end;

procedure TFastIniFile.WriteRect(const Section, Ident: string;
  const Value: TRect);
begin
  WriteString(Section, Ident, RectToStr(Value));
end;

function TFastIniFile.ReadSimplePassword(const Section, Ident: string;
  Default: string): string;
var
  I: Integer;
  Idx: Integer;
  TheData: Byte;
  XSeed: Byte;
  XMod: Byte;
  Password: string;
begin
  XSeed  := FPasswordSeed;
  XMod   := FPasswordMod;
  Result := '';

  Password := ReadString(Section, Ident, Default);
  if Length(Password) > 1 then
    for I := 0 to (Length(Password) - 1) div 2 do
    begin
      Idx := 2 * I;
      TheData := (Ord(Password[Idx + 1]) and $0F) or
                 ((Ord(Password[Idx + 2]) and $0F) shl $04);
      Result  := Result + Char(TheData xor XSeed);
      XSeed   := (XSeed + XMod) and $FF;
    end;
end;

procedure TFastIniFile.WriteSimplePassword(const Section, Ident: string;
  Value: string);
var
  I: Integer;
  TheData: Byte;
  XSeed: Byte;
  XMod: Byte;
  Password: string;
begin
  XSeed    := FPasswordSeed;
  XMod     := FPasswordMod;
  Password := '';

  for I := 1 to Length(Value) do
  begin
    TheData  := Ord(Value[I]) xor XSeed;
    Password := Password + Char((TheData and $0F) or $40);
    Password := Password + Char((TheData shr $04) or $40);
    XSeed    := (XSeed + XMod) and $FF;
  end;
  WriteString(Section, Ident, Password);
end;

procedure TFastIniFile.ReadStrings(const Section, Ident: string; S: TStrings);
var
  Count: Integer;
  I: Integer;
begin
  Count := ReadInteger(Section, Ident + 'Count', 0);
  S.Clear;
  S.BeginUpdate;
  try
    for I := 0 to Count - 1 do
      S.Add(ReadString(Section, Ident + IntToStr(I), ''));
  finally
    S.EndUpdate;
  end;
end;

procedure TFastIniFile.WriteStrings(const Section, Ident: string; S: TStrings);
var
  I: Integer;
  M: Boolean;
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    M := Modified;
    // Get all previous entries for later comparison and remove them.
    ReadStrings(Section, Ident, SL);
    for I := 0 to SL.Count - 1 do
      DeleteKey(Section, Ident + IntToStr(I));

    // Write new entries.
    WriteInteger(Section, Ident + 'Count', S.Count);
    for I := 0 to S.Count - 1 do
      WriteString(Section, Ident + IntToStr(I), S[I]);

    // If there were no changes made before this procedure, check if the
    // the inifile should be rewritten after writing the new entries.
    if not M then
      FModified := CompareStrings(SL, S) <> 0;
  finally
    SL.Free;
  end;
end;


function TFastIniFile.ReadFixedDate(const Section, Ident: string;
  Default: TDateTime): TDateTime;
begin
  Result := StrToDateDef(ReadString(Section, Ident, ''), Default);
end;

function TFastIniFile.ReadFixedDateTime(const Section, Ident: string;
  Default: TDateTime): TDateTime;
var
  D: TDateTime;
  I: Integer;
  S: string;
  T: TDateTime;
begin
  Result := Default;
  S := ReadString(Section, Ident, '');
  I := Pos(' ', S);
  if I > 0 then
  begin
    D := StrToDateDef(Copy(S, 1, Length(FIXED_DATE)), -1);
    T := StrToTimeDef(Copy(S, I+1, Length(FIXED_TIME)), 0);
    if (D <> -1) and (T <> -1) then
      Result := D + T;
  end;
end;

function TFastIniFile.ReadFixedTime(const Section, Ident: string;
  Default: TDateTime): TDateTime;
begin
  Result := StrToTimeDef(ReadString(Section, Ident, ''), Default);
end;

procedure TFastIniFile.WriteFixedDate(const Section, Ident: string;
  Value: TDateTime);
begin
  WriteString(Section, Ident, FormatDateTime(FIXED_DATE, Value))
end;

procedure TFastIniFile.WriteFixedDateTime(const Section, Ident: string;
  Value: TDateTime);
begin
  WriteString(Section, Ident, FormatDateTime(FIXED_DATETIME, Value))
end;

procedure TFastIniFile.WriteFixedTime(const Section, Ident: string;
  Value: TDateTime);
begin
  WriteString(Section, Ident, FormatDateTime(FIXED_TIME, Value))
end;


{ TFastIniStream }

constructor TFastIniStream.Create(Stream: TStream);
begin
  inherited Create('');
  FStream := Stream;
end;

procedure TFastIniStream.FlushBuffers;
begin
  if Modified then
  begin
    FStream.Size := 0;
{$IFDEF COMPILER12_UP}
    FIniItems.SaveToStream(FStream, FEncoding);
{$ELSE}
    FIniItems.SaveToStream(FStream);
{$ENDIF}
  end;
  Modified := False;
end;

procedure TFastIniStream.LoadValues;
begin
  // Note: NO inherited because that one will attempt
  //       to read a file or Clear the data.
  if Assigned(FStream) then
  begin
    FStream.Position := 0;
{$IFDEF COMPILER12_UP}
    FIniItems.LoadFromStream(FStream, FEncoding);
{$ELSE}
    FIniItems.LoadFromStream(FStream);
{$ENDIF}
  end
  else
    Clear;
  Loaded := True;
end;


procedure SetupLookupTables;
var
  I: Char;
begin
  for I := Low(I) to High(I) do
    if (I >= 'a') and (I <= 'z') then
      UpperCaseLookUp[I] := Char(Ord(I) - 32)
    else
      UpperCaseLookUp[I] := I;

  for I := Low(I) to High(I) do
    AnsiUpperCaseLookUp[I] := I;
{$IFNDEF LINUX}
  CharUpperBuff(AnsiUpperCaseLookUp, Length(AnsiUpperCaseLookUp));
{$ENDIF}
end;

initialization
  SetupLookupTables;
end.
