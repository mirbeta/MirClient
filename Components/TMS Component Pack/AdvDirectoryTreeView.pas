{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2015                                        }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The complete source code remains property of the author and may    }
{ not be distributed, published, given or sold in any form as such.  }
{ No parts of the source code can be included in any other component }
{ or application without written authorization of the author.        }
{********************************************************************}

unit AdvDirectoryTreeView;

interface

{$I TMSDEFS.INC}

{$IFDEF VCLLIB}
{$IFDEF DELPHIXE2_LVL}
{$DEFINE USEUITYPES}
{$DEFINE USEOLDERVCL}
{$ENDIF}
{$ENDIF}

{$IFDEF FMXLIB}
{$DEFINE USEUITYPES}
{$ENDIF}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Classes, AdvTreeView, AdvTreeViewData,
  AdvTreeViewBase{$IFNDEF LCLLIB}, Generics.Collections{$ELSE}, fgl{$ENDIF}
  {$IFDEF USEUITYPES}
  , UITypes
  {$ENDIF}
  {$IFDEF FMXLIB}
  , FMX.Graphics, AdvBaseControl
  {$ENDIF}
  {$IFDEF VCLLIB}
  , Controls
  {$ENDIF}
  ;

type
  TAdvDirectoryTreeViewNode = class(TAdvTreeViewNode)
  private
    FFileName: String;
  protected
    function CreateNodes: TAdvTreeViewNodes; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property FileName: String read FFileName write FFilename;
  end;

  TAdvDirectoryTreeViewNodes = class(TAdvTreeViewNodes)
  protected
    function GetItemClass: TCollectionItemClass; override;
  end;

  TAdvDirectoryTreeViewColumnKind = (tvckName, tvckDisplayName, tvckCreationDate, tvckModificationDate, tvckFreeSpace, tvckTotalSize, tvckFreeSpaceAndTotalSize, tvckCustom);

  TAdvDirectoryTreeViewColumn = class(TAdvTreeViewColumn)
  private
    FKind: TAdvDirectoryTreeViewColumnKind;
    procedure SetKind(const Value: TAdvDirectoryTreeViewColumnKind);
  public
    constructor Create(Collection: TCollection); override;
  published
    property Kind: TAdvDirectoryTreeViewColumnKind read FKind write SetKind default tvckName;
  end;

  TAdvDirectoryTreeViewColumns = class(TAdvTreeViewColumns)
  protected
    function GetItemClass: TCollectionItemClass; override;
  end;

  TAdvDirectoryTreeViewGetCustomNodeTextEvent = procedure(Sender: TObject; ANode: TAdvTreeViewNode; AColumn: Integer; AFile: String; var AText: String) of object;

  TAdvDirectoryTreeViewOption = (tvoFiles, tvoHidden, tvoDirectories);
  TAdvDirectoryTreeViewOptions = set of TAdvDirectoryTreeViewOption;

const
  AllDirectoryOptions = [tvoFiles, tvoHidden, tvoDirectories];

type

  TAdvDirectoryTreeViewMode = (dtvmNone, dtvmDrives, dtvmDrive, dtvmFileName);

  {$IFDEF FMXLIB}
  [ComponentPlatformsAttribute(TMSTreeViewPlatformsDesktop)]
  {$ENDIF}
  {$IFDEF VCLLIB}
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  {$ENDIF}
  TAdvDirectoryTreeView = class(TAdvTreeView)
  private
    FRootMode: TAdvDirectoryTreeViewMode;
    FRootFileName: String;
    FOldCursor: TCursor;
    FOnGetCustomNodeText: TAdvDirectoryTreeViewGetCustomNodeTextEvent;
    FFilter: String;
    FOptions: TAdvDirectoryTreeViewOptions;
    procedure SetFilter(const Value: String);
  protected
    function CreateColumns: TAdvTreeViewColumns; override;
    function CreateNodes: TAdvTreeViewNodes; override;
    procedure DoGetCustomNodeText(ANode: TAdvTreeViewNode; AColumn: Integer; AFile: String; var AText: String); virtual;
    procedure GetDriveLetters(AList: TStringList); virtual;
    procedure DoBeforeExpandNode(ANode: TAdvTreeViewVirtualNode; var ACanExpand: Boolean); override;
    procedure AddNodeData(ANode: TAdvTreeViewNode; AFile: String); virtual;
    procedure AddSubNodes(ANode: TAdvTreeViewNode); virtual;
    procedure AddNodes(APath: String; AParentNode: TAdvTreeViewNode = nil; AExpand: Boolean = False); virtual;
    procedure SetWaitCursor; virtual;
    procedure SetOldCursor; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function AddColumn(AKind: TAdvDirectoryTreeViewColumnKind): TAdvDirectoryTreeViewColumn; virtual;
    procedure InitSample; override;
    procedure LoadDirectory(ADirectory: String; AExpand: Boolean = False); virtual;
    procedure LoadDrives(AExpand: Boolean = False); virtual;
    procedure LoadDrive(ADrive: String; AExpand: Boolean = False); virtual;
  published
    property OnGetCustomNodeText: TAdvDirectoryTreeViewGetCustomNodeTextEvent read FOnGetCustomNodeText write FOnGetCustomNodeText;
    property Options: TAdvDirectoryTreeViewOptions read FOptions write FOptions default AllDirectoryOptions;
    property Filter: String read FFilter write SetFilter;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  ShellApi,
  {$ENDIF}
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  MacApi.AppKit, MacApi.Foundation, MacApi.Helpers, MacApi.ObjectiveC,
  MacApi.CocoaTypes,
  {$ENDIF}
  {$ENDIF}
  SysUtils, DateUtils
  {$IFDEF VCLLIB}
  ,Forms, Graphics
  {$ENDIF}
  {$IFDEF FMXLIB}
  ,FMX.Types, FMX.Platform
  {$ENDIF}
  ;


{$IFDEF MACOS}
{$IFNDEF IOS}
const
  libF = '/System/Library/Frameworks/Foundation.framework/Foundation';

function NSFCreationDate: NSString;
begin
  Result := CocoaNSStringConst(libF, 'NSFileCreationDate');
end;

function NSFModificationDate: NSString;
begin
  Result := CocoaNSStringConst(libF, 'NSFileModificationDate');
end;

function NSFileType: NSString;
begin
  Result := CocoaNSStringConst(libF, 'NSFileType');
end;

function NSFileSystemFreeSize: NSString;
begin
  Result := CocoaNSStringConst(libF, 'NSFileSystemFreeSize');
end;

function NSFileSystemSize: NSString;
begin
  Result := CocoaNSStringConst(libF, 'NSFileSystemSize');
end;

function NSFileSize: NSString;
begin
  Result := CocoaNSStringConst(libF, 'NSFileSize');
end;

function CreateThumbNail(AImage: NSImage; AWidth: Single): NSImage;
var
  imgsz: NSSize;
  aspectRatio: CGFloat;
  thumbsz: NSSize;
  r: NSRect;
  rc: NSRect;
begin
  Result := nil;
  if Assigned(AImage) then
  begin
    imgsz := AImage.size;
    aspectratio := imgsz.width / imgsz.height;
    thumbsz.width := AWidth;
    thumbsz.height := AWidth * aspectRatio;
    Result := TNSImage.Wrap(TNSImage.Wrap(TNSImage.OCClass.alloc).initWithSize(thumbsz));
    Result.lockFocus;
    r.origin.x := 0;
    r.origin.y := 0;
    r.size.width := thumbsz.width;
    r.size.height := thumbsz.height;
    rc.origin.x := 0;
    rc.origin.y := 0;
    rc.size.width := imgsz.width;
    rc.size.height := imgsz.height;
    AImage.drawInRect(r, rc, NSCompositeSourceOver, 1.0);
    Result.unlockFocus;
  end;
end;

function BitmapFromImage(AImage: NSImage): TAdvTreeViewBitmap;
var
  dt: NSData;
  ms: TMemoryStream;
begin
  Result := nil;
  if not Assigned(AImage) then
    Exit;

  dt := AImage.TIFFRepresentation;
  if Assigned(dt) then
  begin
    Result := TAdvTreeViewBitmap.Create;
    ms := TMemoryStream.Create;
    ms.Write(dt.bytes^, dt.length);
    ms.Position := 0;
    Result.LoadFromStream(ms);
    ms.Free;
  end;
end;
{$ENDIF}
{$ENDIF}

{$IFNDEF LCLLIB}
{$IFDEF MSWINDOWS}
const
  Zero = 0;
  One = 1;
  Two = 2;
  Four = 4;
  n16 = 16;
  green = $FF00;
  white = $FFFFFF;

  AryPalColor: Array [Byte] of Cardinal = (Zero, $800000, $008000, $808000,
    $000080, $800080, $008080, $C0C0C0, $C0DCC0, $A6CAF0, $D4F0FF, $B1E2FF,
    $8ED4FF, $6BC6FF, $48B8FF, $00ACFF, $009FEE, $0092DC, $007AB9, $006296,
    $004A73, $003250, $D4E3FF, $B1C7FF, $8EABFF, $6B8FFF, $4A79FF, $0069FF,
    $0058EE, $0049DC, $003DB9, $003196, $002573, $001950, $D4D4FF, $B1B1FF,
    $8E8EFF, $6B6BFF, $4848FF, $0000EE, $0000DE, $0000CC, $0000B6, $000098,
    $00007A, $000058, $E3D4FF, $C7B1FF, $AB8EFF, $8F6BFF, $784AFF, $7600FF,
    $6F00EE, $6000D6, $3D00B9, $310096, $250073, $190050, $F0D4FF, $E2B1FF,
    $D48EFF, $C66BFF, $B848FF, $AC00FF, $9F00EE, $9200DC, $7A00B9, $620096,
    $4A0073, $320050, $FFD4FF, $FFB1FF, $FF8EFF, $FF6BFF, $FF48FF, $EE00EE,
    $DE00DE, $CC00CC, $B600B6, $980098, $790079, $490049, $FFD4F0, $FFB1E2,
    $FF8ED4, $FF6BC6, $FF48B8, $FF00AA, $EE009F, $DC0092, $B9007A, $960062,
    $73004A, $500032, $FFD4E3, $FFB1C7, $FF8EAB, $FF6B8F, $FF4873, $FF0066,
    $EE0059, $DC0049, $B9003D, $960031, $730025, $500019, $FFD4D4, $FFB1B1,
    $FF8E8E, $FF6B6B, $FF4848, $EF0000, $DE0000, $CC0000, $B80000, $9F0000,
    $7C0000, $580000, $FFE3D4, $FFC7B1, $FFAB8E, $FF8F6B, $FF7348, $FF6600,
    $EE5800, $DC4900, $B93D00, $963100, $732500, $501900, $FFF0D4, $FFE2B1,
    $FFD48E, $FFC66B, $FFBA48, $FFB100, $EEA300, $DC9200, $B97A00, $966200,
    $734A00, $503200, $FFFFD4, $FFFFB1, $FFFF8E, $FFFF6B, $FFFF48, $EEEE00,
    $DEDE00, $CCCC00, $B6B600, $989800, $747400, $484800, $F0FFD4, $E2FFB1,
    $D4FF8E, $C6FF6B, $B8FF48, $AAFF00, $9FED00, $92DC00, $7AB900, $629600,
    $4A7300, $325000, $E3FFD4, $C7FFB1, $ABFF8E, $8FFF6B, $73FF48, $66FF00,
    $58EE00, $49DC00, $3DB900, $319600, $257300, $194C00, $D4FFD4, $B1FFB1,
    $8EFF8E, $6BFF6B, $48FF48, $00EF00, $00DC00, $00C900, $00B000, $009400,
    $007600, $005200, $D4FFE3, $B1FFC7, $8EFFAB, $6BFF8F, $48FF73, $00FF66,
    $00EB5A, $00DC49, $00B93D, $009631, $007325, $005019, $D4FFF0, $B1FFE2,
    $8EFFD4, $6BFFC6, $48FFB8, $00FFAA, $00EAA0, $00DC92, $00B97A, $009662,
    $00734A, $005032, $D4FFFF, $B1FFFF, $8EFFFF, $6BFFFF, $48FFFF, $00EEEE,
    $00DEDE, $00CCCC, $00B6B6, $009898, $007979, $004A4A, $F2F2F2, $E6E6E6,
    $DADADA, $CECECE, $C2C2C2, $B6B6B6, $AAAAAA, $9E9E9E, $929292, $868686,
    $7A7A7A, $6E6E6E, $626262, $565656, $4A4A4A, $3E3E3E, $323232, $262626,
    $1A1A1A, $0E0E0E, $FFFBF0, $A0A0A4, $808080, $FF0000, green, $FFFF00,
    MaxByte, $FF00FF, $00FFFF, white);

type
  TBitCount = (BitC1, BitC4, BitC8, BitC24, BitC32);
  PMutiIcon = ^TMutiIcon;

  TMutiIcon = Record
    hIcon: Cardinal;
    BitCount: TBitCount;
  end;

type
  PIconCurHeader = ^TIconCurHeader;

  TIconCurHeader = packed record
    wReserved: Word;
    wType: Word;
    wCount: Word;
  end;

  PIconSpec = ^TIconSpec;

  TIconSpec = packed record
    bWidth: Byte;
    bHeight: Byte;
    wColors: Word;
    wReserved1: Word;
    wReserved2: Word;
    iDIBSize: Integer;
    iDIBOffset: Integer;
  end;

function SetInfo(var BmpInfoHead: TBitmapInfoHeader; Width, Height: Integer;
  BitCnt: TBitCount): Cardinal;
begin
  ZeroMemory(@BmpInfoHead, sizeof(BmpInfoHead));

  with BmpInfoHead do
  begin
    biSize := sizeof(TBitmapInfoHeader);
    biWidth := Width;
    biHeight := Height;
    biPlanes := One;
    biClrUsed := Zero;

    case BitCnt of
      BitC1:
        begin
          biBitCount := One;
          biClrUsed := Two;
        end;
      BitC4:
        begin
          biBitCount := Four;
          biClrUsed := n16;
        end;
      BitC8:
        begin
          biBitCount := 8;
          biClrUsed := 256;
        end;
      BitC32:
        biBitCount := 32;
    else
      biBitCount := 24;
    end;

    biClrImportant := biClrUsed;

    biSizeImage := ((biWidth * biBitCount) + 31) and not 31;
    biSizeImage := Integer((biSizeImage shr 3)) * Abs(biHeight);

    Result := sizeof(TBitmapInfoHeader);
    if biBitCount < 9 then
      Result := Result + (biClrUsed shl Two);
  end;
end;

function Do1BitDib(var hBmp: HBITMAP): Cardinal;
const
  AryColor: Array [Zero .. One] of Cardinal = (Zero, white);
var
  BmpInfo1: TBitmapInfoHeader;
  pBits: Pointer;
  DC1, HeaderSize, hNewBmp, inDC, outDC: Cardinal;
  tBmp1: tagBitmap;
  PntBmpInfo: Pointer;
begin
  Result := Zero;
  ZeroMemory(@BmpInfo1, sizeof(BmpInfo1));
  if GetObject(hBmp, sizeof(tBmp1), @tBmp1) = Zero then
    Exit;
  tBmp1.bmHeight := tBmp1.bmHeight shr One;
  HeaderSize := SetInfo(BmpInfo1, tBmp1.bmWidth, tBmp1.bmHeight, BitC1);
  if HeaderSize = Zero then
    Exit;
  GetMem(PntBmpInfo, HeaderSize);
  CopyMemory(PntBmpInfo, @BmpInfo1, sizeof(BmpInfo1));
  CopyMemory(@TBitmapInfo(PntBmpInfo^).bmiColors, @AryColor[Zero], 8);

  pBits := nil;
  DC1 := CreateCompatibleDC(Zero);
  try
    Result := CreateDIBSection(DC1, TBitmapInfo(PntBmpInfo^), DIB_RGB_COLORS,
      pBits, Zero, Zero);
    if Result = Zero then
      Exit;

    hNewBmp := CreateDIBSection(DC1, TBitmapInfo(PntBmpInfo^), DIB_RGB_COLORS,
      pBits, Zero, Zero);
    if hNewBmp = Zero then
    begin
      DeleteObject(Result);
      Result := Zero;
      Exit;
    end;
    SelectObject(DC1, Result);
    inDC := CreateCompatibleDC(Zero);
    outDC := CreateCompatibleDC(Zero);
    try
      SelectObject(outDC, hNewBmp);
      SelectObject(inDC, hBmp);
      BitBlt(outDC, Zero, Zero, tBmp1.bmWidth, tBmp1.bmHeight, inDC, Zero,
        Zero, SRCCOPY);
      BitBlt(DC1, Zero, Zero, tBmp1.bmWidth, tBmp1.bmHeight, inDC, Zero,
        tBmp1.bmHeight, SRCCOPY);

    finally
      DeleteDC(inDC);
      DeleteObject(hBmp);
      DeleteDC(outDC);
    end;
    hBmp := hNewBmp;
  finally
    FreeMem(PntBmpInfo);
    DeleteDC(DC1);
  end;
end;

function GetInfoAndBits(HBITMAP: Cardinal; var PntBmpInfo: Pointer;
  var PntBits: Pointer; BitCount: TBitCount; var hBmp8: Cardinal): Cardinal;
var
  HeaderSize, cDC1, inDC: Cardinal;
  BmpInfo1: TBitmapInfoHeader;
  tBmp1: tagBitmap;
begin
  Result := Zero;
  if GetObject(HBITMAP, sizeof(tBmp1), @tBmp1) = Zero then
    Exit;

  HeaderSize := SetInfo(BmpInfo1, tBmp1.bmWidth, tBmp1.bmHeight, BitCount);
  GetMem(PntBmpInfo, HeaderSize);
  CopyMemory(PntBmpInfo, @BmpInfo1, sizeof(BmpInfo1));

  hBmp8 := Zero;
  cDC1 := CreateCompatibleDC(Zero);

  try
    if BitCount = BitC8 then
    begin
      CopyMemory(@TBitmapInfo(PntBmpInfo^).bmiColors, @AryPalColor[Zero], 1024);
      hBmp8 := CreateDIBSection(cDC1, TBitmapInfo(PntBmpInfo^), DIB_RGB_COLORS,
        PntBits, Zero, Zero);
      if hBmp8 = Zero then
      begin
        Exit;
      end
      else
        Result := HeaderSize;
      SelectObject(cDC1, hBmp8);
      inDC := CreateCompatibleDC(Zero);
      GdiFlush;
      SelectObject(inDC, HBITMAP);
      if not BitBlt(cDC1, Zero, Zero, tBmp1.bmWidth, tBmp1.bmHeight, inDC, Zero,
        Zero, SRCCOPY) then
        Result := Zero;
      DeleteDC(inDC);
    end
    else
    begin
      GetMem(PntBits, BmpInfo1.biSizeImage);
      if GetDIBits(cDC1, HBITMAP, Zero, Abs(BmpInfo1.biHeight), PntBits,
        TBitmapInfo(PntBmpInfo^), DIB_RGB_COLORS) <> Zero then
        Result := HeaderSize;
    end;

  finally
    DeleteDC(cDC1);
  end;
end;

function IconsToStream(Stream: TStream; aryMutiIcon: Array of TMutiIcon;
  NumIcons: Byte = One): Integer;
var
  i, cIHeaderSize, mIHeaderSize, hBmp8bit, EndPos, StartoF, zip: Cardinal;
  IconInfo: TIconInfo;
  PcBits, PcInfo, PmBits, PmInfo, Pstorage: Pointer;
  IconCurHd: TIconCurHeader;
  IconSpec: TIconSpec;
begin
  Result := -One;
  if (not Assigned(Stream)) then
    Exit;
  if NumIcons = Zero then
  begin
    Result := -2;
    Exit;
  end;
  if NumIcons > 15 then
  begin
    Result := -3;
    Exit;
  end;
  if High(aryMutiIcon) < NumIcons - One then
  begin
    Result := -4;
    Exit;
  end;

  IconCurHd.wReserved := Zero;
  IconCurHd.wType := One;
  IconCurHd.wCount := NumIcons;
  EndPos := Zero;
  Pstorage := nil;

  try
    for i := Zero to NumIcons - One do
    begin
      if not GetIconInfo(aryMutiIcon[i].hIcon, IconInfo) then
      begin
        Result := -5;
        Exit;
      end;
      try
        if IconInfo.hbmColor < 33 then
        begin
          if IconInfo.hbmMask > 32 then
            IconInfo.hbmColor := Do1BitDib(IconInfo.hbmMask);
          if IconInfo.hbmColor < 33 then
          begin
            Result := -6;
            Exit;
          end;
        end;

        PcInfo := nil;
        PcBits := nil;
        PmInfo := nil;
        PmBits := nil;
        hBmp8bit := Zero;

        cIHeaderSize := GetInfoAndBits(IconInfo.hbmColor, PcInfo, PcBits,
          aryMutiIcon[i].BitCount, hBmp8bit);

        mIHeaderSize := GetInfoAndBits(IconInfo.hbmMask, PmInfo, PmBits,
          BitC1, zip);
        try
          if (cIHeaderSize = Zero) or (mIHeaderSize = Zero) then
          begin
            Result := -7;
            Exit;
          end;

          ZeroMemory(@IconSpec, sizeof(IconSpec));
          with IconSpec, PBitmapInfo(PcInfo).bmiHeader do
          begin
            bWidth := biWidth;
            bHeight := biHeight;
            wColors := biBitCount;

            iDIBOffset := sizeof(IconCurHd) + sizeof(IconSpec);
            iDIBSize := cIHeaderSize + biSizeImage + PBitmapInfo(PmInfo)
              .bmiHeader.biSizeImage;
            biHeight := bHeight shl One;
          end;
          if i = Zero then
          begin
            EndPos := sizeof(IconCurHd) + (NumIcons * sizeof(IconSpec)) +
              cIHeaderSize + PBitmapInfo(PcInfo).bmiHeader.biSizeImage +
              PBitmapInfo(PmInfo).bmiHeader.biSizeImage;
            ReAllocMem(Pstorage, EndPos);
            CopyMemory(Pstorage, @IconCurHd, sizeof(IconCurHd));
            StartoF := sizeof(IconCurHd) + (NumIcons * sizeof(IconSpec));
          end
          else
          begin
            StartoF := EndPos;
            EndPos := EndPos + cIHeaderSize + PBitmapInfo(PcInfo)
              .bmiHeader.biSizeImage + PBitmapInfo(PmInfo)
              .bmiHeader.biSizeImage;
            ReAllocMem(Pstorage, EndPos);
          end;
          CopyMemory(Pointer(Cardinal(Pstorage) + StartoF), PcInfo,
            cIHeaderSize);
          CopyMemory(Pointer(Cardinal(Pstorage) + StartoF + cIHeaderSize),
            PcBits, PBitmapInfo(PcInfo).bmiHeader.biSizeImage);
          CopyMemory(Pointer(Cardinal(Pstorage) + StartoF + cIHeaderSize +
            PBitmapInfo(PcInfo).bmiHeader.biSizeImage), PmBits,
            PBitmapInfo(PmInfo).bmiHeader.biSizeImage);
          with IconSpec, PBitmapInfo(PcInfo).bmiHeader do
          begin
            bWidth := biWidth;
            bHeight := biHeight shr One;
            wColors := biPlanes * biBitCount;
            iDIBSize := cIHeaderSize + biSizeImage + PBitmapInfo(PmInfo)
              .bmiHeader.biSizeImage;
            iDIBOffset := StartoF;
          end;
          CopyMemory(Pointer(Cardinal(Pstorage) + sizeof(IconCurHd) +
            ((i) * sizeof(IconSpec))), @IconSpec, sizeof(IconSpec));

        finally
          if hBmp8bit <> Zero then
            DeleteObject(hBmp8bit);
          if (PcBits <> nil) and (aryMutiIcon[i].BitCount <> BitC8) then
            FreeMem(PcBits);
          if PmBits <> nil then
            FreeMem(PmBits);
          if PcInfo <> nil then
            FreeMem(PcInfo);
          if PmInfo <> nil then
            FreeMem(PmInfo);
        end;
      finally
        DeleteObject(IconInfo.hbmColor);
        DeleteObject(IconInfo.hbmMask);
      end;
    end;
    Result := Stream.Write(Pstorage^, EndPos);
    if Cardinal(Result) <> EndPos then
      Result := -Result;
  finally
    if Pstorage <> nil then
      FreeMem(Pstorage);
  end;
end;

{$ENDIF}
{$ENDIF}

function AddBackslash(const s: string): string;
begin
  {$IFDEF DELPHI_LLVM}
  if (Length(s) >= 1) and (s[Length(s) - 1] <> '\') and (s[Length(s) - 1] <> '/') then
  {$ELSE}
  if (Length(s) >= 1) and (s[Length(s)] <> '\') and (s[Length(s)] <> '/') then
  {$ENDIF}
    Result := s + '\'
  else
    Result := s;
end;

{ TAdvDirectoryTreeView }

function TAdvDirectoryTreeView.AddColumn(
  AKind: TAdvDirectoryTreeViewColumnKind): TAdvDirectoryTreeViewColumn;
begin
  Result := TAdvDirectoryTreeViewColumn(Columns.Add);
  Result.Kind := AKind;
end;

procedure TAdvDirectoryTreeView.AddNodeData(ANode: TAdvTreeViewNode; AFile: String);
var
  bmp: TAdvTreeViewBitmap;
  I: Integer;
  fs, fst: Extended;
  n, dn, cd, md: String;
  {$IFDEF MSWINDOWS}
  {$IFDEF VCLLIB}
  ic: TIcon;
  {$ENDIF}
  fi: SHFILEINFO;
  fad: TWin32FileAttributeData;
  st: TSystemTime;
  {$IFDEF FMXLIB}
  mi: TMutiIcon;
  ms: TMemoryStream;
  {$ENDIF}
  {$ENDIF}
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  ic, icm: NSImage;
  ws: NSWorkspace;
  dic: NSDictionary;
  fm: NSFileManager;
  p: NSString;
  t: NSString;
  d: NSDate;
  fstn: NSNumber;
  pt: Pointer;
  ft: NSString;
  {$ENDIF}
  {$ENDIF}
  ct: String;
  dir: Boolean;
  arrbmp: TAdvTreeViewNodeIcons;

  function GetAvailableSpace(ASize: Extended): String;
  const
    Measure: Array[0..4] of String = ('Bytes', 'KB', 'MB', 'GB', 'TB');
  var
    NewSize: Extended;
    i: Integer;
  begin
    Result := '';

    if ASize > -1 then
    begin
      i := 0;
      NewSize := ASize;

      while (NewSize >= 1024) do
      begin
        NewSize := NewSize / 1024;
        Inc(i);
      end;

      case i of
        0: Result := FloatToStr(NewSize);
        1, 2, 3, 4: Result := FormatFloat('0.00', NewSize);
      else
        Result := FloatToStr(NewSize);
      end;

      Result := Result + #32 + Measure[i];
    end;
  end;
begin
  bmp := nil;
  dir := False;
  fs := 0;

  {$IFDEF DELPHI_LLVM}
  fst := 0;
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  SHGetFileInfo(PChar(AFile), 0, fi, SizeOf(fi), SHGFI_DISPLAYNAME or SHGFI_TYPENAME or SHGFI_ICON or SHGFI_SMALLICON);
  GetFileAttributesEx(PChar(AFile), GetFileExInfoStandard, @fad);
  n := fi.szDisplayName;
  dn := fi.szTypeName;
  if ExtractFileDrive(AFile) + PathDelim = AFile then
  begin
    dir := True;
    //drive
    fst := DiskSize(Ord(UpperCase(AFile)[1]) - 64);
    fs := DiskFree(Ord(UpperCase(AFile)[1]) - 64);
  end
  else
    fst := Int64(fad.nFileSizeLow) or Int64(fad.nFileSizeHigh shl 32);

  FileTimeToSystemTime(fad.ftCreationTime, st);
  cd := DateTimeToStr(EncodeDateTime(st.wYear, st.wMonth, st.wDay, st.wHour, st.wMinute, st.wSecond, st.wMilliseconds));
  FileTimeToSystemTime(fad.ftLastWriteTime, st);
  md := DateTimeToStr(EncodeDateTime(st.wYear, st.wMonth, st.wDay, st.wHour, st.wMinute, st.wSecond, st.wMilliseconds));
  if (fi.hIcon <> 0) and not Assigned(bmp) then
  begin
    bmp := TAdvTreeViewBitmap.Create;
    {$IFDEF VCLLIB}
    ic := TIcon.Create;
    ic.Handle := fi.hIcon;
    bmp.Assign(ic);
    ic.Free;
    {$ENDIF}
    {$IFDEF FMXLIB}
    ms := TMemoryStream.Create;
    mi.hIcon := fi.hIcon;
    mi.BitCount := BitC32;
    IconsToStream(ms, [mi]);
    bmp.LoadFromStream(ms);
    ms.Free;
    {$ENDIF}
  end;
  {$ENDIF}

  {$IFDEF MACOS}
  {$IFNDEF IOS}
  fst := 0;
  p := StrToNSStr(AFile);
  fm := TNSFileManager.Wrap(TNSFileManager.OCClass.defaultManager);
  ws := TNSWorkspace.Wrap(TNSWorkspace.OCClass.sharedWorkspace);
  ic := ws.iconForFile(p);
  dic := fm.attributesOfItemAtPath(p, nil);
  t := ws.localizedDescriptionForType(ws.typeOfFile(p, nil));
  if Assigned(t) then
    dn := UTF8ToString(t.UTF8String);

  t := fm.displayNameAtPath(p);
  if Assigned(t) then
    n := UTF8ToString(t.UTF8String);

  if Assigned(dic) then
  begin
    pt := dic.objectForKey((NSFCreationDate as ILocalObject).GetObjectID);
    if Assigned(pt) then
    begin
      d := TNSDate.Wrap(pt);
      if Assigned(d) then
        cd := UTF8ToString(d.description.UTF8String);
    end;

    pt := dic.objectForKey((NSFModificationDate as ILocalObject).GetObjectID);
    if Assigned(pt) then
    begin
      d := TNSDate.Wrap(pt);
      if Assigned(d) then
        md := UTF8ToString(d.description.UTF8String);
    end;

    ft := nil;
    pt := dic.objectForKey((NSFileType as ILocalObject).GetObjectID);
    if Assigned(pt) then
      ft := TNSString.Wrap(pt);

    if UpperCase(dn) = 'VOLUME' then
    begin
      dir := True;
      dic := fm.attributesOfFileSystemForPath(p, nil);
      pt := dic.objectForKey((NSFileSystemFreeSize as ILocalObject).GetObjectID);
      if Assigned(pt) then
      begin
        fstn := TNSNumber.Wrap(pt);
        if Assigned(fstn) then
          fs := fstn.unsignedLongLongValue;
      end;

      pt := dic.objectForKey((NSFileSystemSize as ILocalObject).GetObjectID);
      if Assigned(pt) then
      begin
        fstn := TNSNumber.Wrap(pt);
        if Assigned(fstn) then
          fst := fstn.unsignedLongLongValue;
      end;
    end
    else if Assigned(ft) and not (UTF8ToString(ft.UTF8String) = 'NSFileTypeDirectory') then
    begin
      pt := dic.objectForKey((NSFileSize as ILocalObject).GetObjectID);
      if Assigned(pt) then
      begin
        fstn := TNSNumber.Wrap(pt);
        if Assigned(fstn) then
          fst := fstn.unsignedLongLongValue;
      end;
    end;
  end;

  if Assigned(ic) and not Assigned(bmp) then
  begin
    icm := CreateThumbNail(ic, 16);
    bmp := BitmapFromImage(icm);
    icm.release;
  end;
  {$ENDIF}
  {$ENDIF}

  for I := 0 to Columns.Count - 1 do
  begin
    case TAdvDirectoryTreeViewColumn(Columns[I]).Kind of
      tvckName: ANode.Text[I] := n;
      tvckDisplayName: ANode.Text[I] := dn;
      tvckCreationDate: ANode.Text[I] := cd;
      tvckModificationDate: ANode.Text[I] := md;
      tvckFreeSpace:
      begin
        if dir and (fs > 0) then
          ANode.Text[I] := GetAvailableSpace(fs)
        else
          ANode.Text[I] := '';
      end;
      tvckFreeSpaceAndTotalSize:
      begin
        if dir and (fs > 0) and (fst > 0) then
          ANode.Text[I] := GetAvailableSpace(fs) + ' / ' + GetAvailableSpace(fst)
        else if fst > 0 then
          ANode.Text[I] := GetAvailableSpace(fst)
        else
          ANode.Text[I] := '';
      end;
      tvckTotalSize:
      begin
        if fst > 0 then
          ANode.Text[I] := GetAvailableSpace(fst)
        else
          ANode.Text[I] := '';
      end;
      tvckCustom:
      begin
        ct := '';
        DoGetCustomNodeText(ANode, I, AFile, ct);
        ANode.Text[I] := ct;
      end;
    end;
  end;

  SetLength(arrbmp, 1);
  arrbmp[0] := bmp;
  ANode.SetCollapsedIcons(arrbmp);
  ANode.SetExpandedIcons(arrbmp);

  if Assigned(bmp) then
    bmp.Free;
end;

procedure TAdvDirectoryTreeView.AddNodes(APath: String; AParentNode: TAdvTreeViewNode = nil; AExpand: Boolean = False);
var
  sr: TSearchRec;
  i: Integer;
  pn: TAdvTreeViewNode;
  chk: Boolean;
begin
  BeginUpdate;
  APath := AddBackslash(APath);

  i := FindFirst(APath + Filter, faAnyFile, sr);
  while i = 0 do
  begin
    chk := (sr.Name <> '.') and (sr.Name <> '..');
    if not (tvoFiles in Options) then
      chk := chk and (sr.Attr and 16 = 16);

    if not (tvoHidden in Options) then
      chk := chk and (sr.Attr and 2 <> 2);

    if not (tvoDirectories in Options) then
      chk := chk and (sr.Attr and 16 <> 16);

    if chk then
    begin
      pn := AddNode(AParentNode);
      AddNodeData(pn, APath + sr.Name);

      if sr.Attr and faDirectory = faDirectory then
      begin
        TAdvDirectoryTreeViewNode(pn).FileName := APath + sr.Name + PathDelim;
        TAdvDirectoryTreeViewNode(AddNode(pn)).FileName := APath + sr.Name;
        if AExpand then
        begin
          AddSubNodes(pn);
          pn.Expanded := True;
        end;
      end
      else
        TAdvDirectoryTreeViewNode(pn).FileName := APath + sr.Name;
    end;
    i := FindNext(sr);
  end;
  FindClose(sr);
  EndUpdate;
end;

procedure TAdvDirectoryTreeView.AddSubNodes(ANode: TAdvTreeViewNode);
var
  str: string;
begin
  if not Assigned(ANode) then
    Exit;

  if not ANode.DataBoolean then
  begin
    ANode.DataBoolean := True;
    if ANode.Nodes.Count > 0 then
    begin
      str := TAdvDirectoryTreeViewNode(ANode.Nodes[0]).FileName;
      RemoveNode(ANode.Nodes[0]);
      AddNodes(str, ANode);
    end;
  end;
end;

constructor TAdvDirectoryTreeView.Create(AOwner: TComponent);
begin
  inherited;
  FOptions := AllDirectoryOptions;
  FFilter := '*';
  {$IFDEF VCLLIB}
  if IsDesignTime then
  begin
    LoadDrive('C:\', True);
  end;
  {$ENDIF}
end;

function TAdvDirectoryTreeView.CreateColumns: TAdvTreeViewColumns;
begin
  Result := TAdvDirectoryTreeViewColumns.Create(Self);
end;

function TAdvDirectoryTreeView.CreateNodes: TAdvTreeViewNodes;
begin
  Result := TAdvDirectoryTreeViewNodes.Create(Self, nil);
end;

procedure TAdvDirectoryTreeView.DoBeforeExpandNode(
  ANode: TAdvTreeViewVirtualNode; var ACanExpand: Boolean);
begin
  inherited;
  SetWaitCursor;
  if ACanExpand and Assigned(ANode) then
    AddSubNodes(ANode.Node);
  SetOldCursor;
end;

procedure TAdvDirectoryTreeView.DoGetCustomNodeText(
  ANode: TAdvTreeViewNode; AColumn: Integer; AFile: String; var AText: String);
begin
  if Assigned(OnGetCustomNodeText) then
    OnGetCustomNodeText(Self, ANode, AColumn, AFile, AText);
end;

procedure TAdvDirectoryTreeView.InitSample;
var
  c: TAdvDirectoryTreeViewColumn;
begin
  BeginUpdate(True);
  Columns.Clear;
  Interaction.ReadOnly := True;
  c := TAdvDirectoryTreeViewColumn(Columns.Add);
  c.Kind := tvckName;
  EndUpdate;
end;

procedure TAdvDirectoryTreeView.LoadDirectory(ADirectory: String; AExpand: Boolean = False);
begin
  SetWaitCursor;
  FRootMode := dtvmFileName;
  FRootFileName := ADirectory;
  BeginUpdate(True);
  Nodes.Clear;
  AddNodes(ADirectory, nil, AExpand);
  EndUpdate;
  SetOldCursor;
end;

procedure TAdvDirectoryTreeView.GetDriveLetters(AList: TStringList);
{$IFDEF MSWINDOWS}
var
  vDrivesSize: Cardinal;
  vDrives: array[0..128] of Char;
  vDrive: PChar;
begin
  try
    vDrivesSize := GetLogicalDriveStrings(SizeOf(vDrives), vDrives);
    if vDrivesSize = 0 then
      Exit;

    vDrive := vDrives;
    while vDrive^ <> #0 do
    begin
      AList.Add(StrPas(vDrive));
      Inc(vDrive, SizeOf(vDrive));
    end;
  finally
  end;
{$ENDIF}
{$IFDEF MACOS}
{$IFNDEF IOS}
var
  v: NSArray;
  I: Integer;
{$ENDIF}
begin
{$IFNDEF IOS}
  v := TNSFileManager.Wrap(TNSFileManager.OCClass.defaultManager).contentsOfDirectoryAtPath(StrToNSStr('/Volumes'), nil);
  if Assigned(v) then
  begin
    for I := 0 to v.count - 1 do
      AList.Add('/Volumes' + PathDelim + UTF8ToString(TNSString.Wrap(v.objectAtIndex(I)).UTF8String) + PathDelim);
  end;
{$ENDIF}
{$ENDIF}
{$IFDEF ANDROID}
begin
{$ENDIF}
end;

procedure TAdvDirectoryTreeView.LoadDrive(ADrive: String; AExpand: Boolean = False);
var
  pn: TAdvTreeViewNode;
begin
  SetWaitCursor;
  ADrive := AddBackslash(ADrive);
  FRootMode := dtvmDrive;
  FRootFileName := ADrive;
  BeginUpdate(True);
  Nodes.Clear;
  pn := AddNode;
  TAdvDirectoryTreeViewNode(pn).FileName := ADrive;
  AddNodeData(pn, ADrive);
  TAdvDirectoryTreeViewNode(AddNode(pn)).FileName := ADrive;
  if AExpand then
  begin
    AddSubNodes(pn);
    pn.Expanded := True;
  end;
  EndUpdate;
  SetOldCursor;
end;

procedure TAdvDirectoryTreeView.LoadDrives(AExpand: Boolean = False);
var
  I: Integer;
  str: TStringList;
  pn: TAdvTreeViewNode;
  dir: String;
begin
  SetWaitCursor;
  FRootMode := dtvmDrives;
  BeginUpdate(True);
  Nodes.Clear;
  str := TStringList.Create;
  GetDriveLetters(str);
  for I := 0 to str.Count - 1 do
  begin
    dir := AddBackslash(str[I]);
    pn := AddNode;
    TAdvDirectoryTreeViewNode(pn).FileName := dir;
    AddNodeData(pn, dir);
    TAdvDirectoryTreeViewNode(AddNode(pn)).FileName := dir;
    if AExpand then
    begin
      AddSubNodes(pn);
      pn.Expanded := True;
    end;
  end;
  str.Free;
  EndUpdate;
  SetOldCursor;
end;

procedure TAdvDirectoryTreeView.SetFilter(const Value: String);
begin
  FFilter := Value;
  case FRootMode of
    dtvmDrives: LoadDrives;
    dtvmFileName: LoadDirectory(FRootFileName);
    dtvmDrive: LoadDrive(FRootFileName);
  end;
end;

{$IFDEF VCLLIB}
procedure TAdvDirectoryTreeView.SetOldCursor;
begin
  Screen.Cursor := FOldCursor;
end;

procedure TAdvDirectoryTreeView.SetWaitCursor;
begin
  FOldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
end;
{$ENDIF}

{$IFDEF FMXLIB}
procedure TAdvDirectoryTreeView.SetOldCursor;
var
  CS: IFMXCursorService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXCursorService) then
    CS := TPlatformServices.Current.GetPlatformService(IFMXCursorService) as IFMXCursorService;

  if Assigned(CS) then
    CS.SetCursor(FOldCursor);
end;

procedure TAdvDirectoryTreeView.SetWaitCursor;
var
  CS: IFMXCursorService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXCursorService) then
    CS := TPlatformServices.Current.GetPlatformService(IFMXCursorService) as IFMXCursorService;

  if Assigned(CS) then
  begin
    FOldCursor := CS.GetCursor;
    CS.SetCursor(crHourGlass);
  end;
end;
{$ENDIF}

{ TAdvDirectoryTreeViewColumns }

function TAdvDirectoryTreeViewColumns.GetItemClass: TCollectionItemClass;
begin
  Result := TAdvDirectoryTreeViewColumn;
end;

{ TAdvDirectoryTreeViewColumn }

constructor TAdvDirectoryTreeViewColumn.Create(Collection: TCollection);
begin
  inherited;
  FKind := tvckName;
end;

procedure TAdvDirectoryTreeViewColumn.SetKind(
  const Value: TAdvDirectoryTreeViewColumnKind);
begin
  FKind := Value;
  case FKind of
    tvckName: Text := TranslateTextEx('Name');
    tvckDisplayName: Text := TranslateTextEx('Display name');
    tvckCreationDate: Text := TranslateTextEx('Date created');
    tvckModificationDate: Text := TranslateTextEx('Date modified');
    tvckFreeSpace: Text := TranslateTextEx('Free Space');
    tvckTotalSize: Text := TranslateTextEx('Total Size');
    tvckFreeSpaceAndTotalSize: Text := TranslateTextEx('Free Space / Total Size');
  end;
end;

{ TAdvDirectoryTreeViewNode }

procedure TAdvDirectoryTreeViewNode.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TAdvDirectoryTreeViewNode then
    FFileName := (Source as TAdvDirectoryTreeViewNode).FileName;
end;

function TAdvDirectoryTreeViewNode.CreateNodes: TAdvTreeViewNodes;
begin
  Result := TAdvDirectoryTreeViewNodes.Create(TreeView, Self);
end;

{ TAdvDirectoryTreeViewNodes }

function TAdvDirectoryTreeViewNodes.GetItemClass: TCollectionItemClass;
begin
  Result := TAdvDirectoryTreeViewNode;
end;

end.

