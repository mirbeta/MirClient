{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxPSUtl;

interface

{$I cxVer.inc}

uses
  Variants, Classes, Windows, SysUtils, Graphics, Messages, Controls, StdCtrls,
  ComCtrls, Dialogs, CommCtrl, Forms, Menus, ImgList, TypInfo, Registry, Themes,
  IniFiles, {$IFDEF DELPHI16} UITypes,{$ENDIF}
  cxClasses, cxGraphics, dxPSGlbl, dxGDIPlusClasses,
  dxPrnDev, dxCore, cxTextEdit, cxLabel, cxDropDownEdit, cxGeometry, cxButtons,
  cxLookAndFeels, cxControls, ExtCtrls, dxLayoutContainer, cxImage;

const
  // Marlett Font
  CheckMarkIndex = 98;
  CheckTopLeftArcOuterIndex = 99;
  CheckBottomRightArcOuterIndex = 100;
  CheckTopLeftArcInnerIndex = 101;
  CheckBottomRightArcInnerIndex = 102;
  CheckInteriorIndex = 103;

  RadioBeanIndex = 105;
  RadioTopLeftArcOuterIndex = 106;
  RadioBottomRightArcOuterIndex = 107;
  RadioTopLeftArcInnerIndex = 108;
  RadioBottomRightArcInnerIndex = 109;
  RadioInteriorIndex = 110;

  SortUpMarkIndex = 116;
  SortDownMarkIndex = 117;

  // Symbol Font
  PlusSignIndex = 43;
  MinusSignIndex = 45;

  InvalidFileNameChars = '<>:"|/\';

function dxPSFixInvalidFileNameChars(const S: string): string;

function MinMax(A, B, C: Integer): Integer;
function SetLoWord(AValue: Integer; ALoWord: Word): Integer;
function SetHiWord(AValue: Integer; AHiWord: Word): Integer;
function SetLoHiWords(AValue: Integer; ALoWord, AHiWord: Word): Integer;
function ScalePoint(const Pt: TPoint; Numerator, Denominator: Integer): TPoint;
function ArePointsEqual(const Pt1, Pt2: TPoint): Boolean;

// GDI
function GetRgnData(ARgn: HRGN; out ARgnDataHeader: TRgnDataHeader; out ARects: TRects): Integer;
function ExcludeClipRect(DC: HDC; const R: TRect): HRGN; overload;
function ExcludeClipRect(DC: HDC; ALeft, ATop, ARight, ABottom: Integer): HRGN; overload;
function IntersectClipRect(DC: HDC; const R: TRect; AlwaysClip: Boolean = False): HRGN; overload;
function IntersectClipRect(DC: HDC; ALeft, ATop, ARight, ABottom: Integer; AlwaysClip: Boolean = False): HRGN; overload;
function RectVisible(DC: HDC; const ARect: TRect): Boolean;
procedure RestoreClipRgn(DC: HDC; var ARgn: HRGN);

function dxAreBitmapsEqual(ABitmap1, ABitmap2: TBitmap): Boolean;
function dxAreBrushesEqual(ABrush1, ABrush2: TBrush): Boolean;
function dxAreGraphicsEqual(AGraphic1, AGraphic2: TGraphic): Boolean;
function dxAreFontsEqual(AFont1, AFont2: TFont): Boolean; overload;
function dxAreFontsEqual(const AFont1Name: string; AFont1Color: TColor; AFont1Pitch: TFontPitch;
  AFont1Style: TFontStyles; AFont1Height: Integer; AFont2: TFont): Boolean; overload;
function dxArePensEqual(APen1, APen2: TPen): Boolean;
function dxIsTrueTypeFont(AFont: TFont): Boolean;

function IsDisplayDC(DC: HDC): Boolean;
function IsMetafileDC(DC: HDC): Boolean;
function IsPrinterDC(DC: HDC): Boolean;

function PatternBrush: HBRUSH;
procedure ForcePictureToBitmap(APicture: TPicture);

procedure ActivateComboBoxControl(AForm: TCustomForm; AComboBoxControl: TWinControl);
procedure CheckDialogFormHelpContext(AForm: TCustomForm; AHelpBtnHolder: TdxLayoutItem);

function CreateGraphic(AGraphicClass: TGraphicClass): TGraphic;
function IconToBitmap(AnIcon: TIcon): TBitmap;
function CreateArrowBitmap(AUpDownGlyph: TdxUpDownGlyph; AWidth: Integer  = 16;
  AHeight: Integer = 16; AFontSize: Integer = 12): TBitmap;
function CreateDoubleArrowBitmap(AUpDownGlyph: TdxUpDownGlyph; AWidth: Integer  = 16;
  AHeight: Integer = 16; AFontSize: Integer = 8): TBitmap;
function CreateGlyphBitmap(AGlyphIndex: Integer; AWidth: Integer = 16;
  AHeight: Integer = 16; AFontSize: Integer = 12): TBitmap;
procedure DrawBlendedText(ACanvas: TCanvas; const R: TRect; const AText: string; AFont: TFont);
procedure DrawGlyph(DC: HDC; const R: TRect; AGlyph: Byte; ACenter: Boolean = False);
procedure DrawSizeGrip(DC: HDC; R: TRect);
procedure TransparentDraw(DrawDC: HDC; Brush: HBRUSH; const R: TRect; ABitmap: TBitmap);

// System
function CopyDeviceMode(Src: HGLOBAL): HGLOBAL;
function GetDesktopWorkArea: TRect;
function GetLongFileName(const Source: string): string;
function GetMachineName: string;
function GetVolumeName(const APath: string): string;
function IsIntelliMousePresent: Boolean;
function IsNetworkPresent: Boolean;
function IsVolume(const APath: string): Boolean;
function PopulateShellImages(FullInit: Boolean): Boolean;
function ShellLargeImages: TCustomImageList;
function ShellSmallImages: TCustomImageList;
function ShowSystemSelectFolderDlg(var ADirPath: string): Boolean;
function FormatFileSize(const AFileSize: Int64): string;
function ValidateFileName(const FileName: string): Boolean;
procedure Delay(Value: DWORD); //milliseconds

procedure MessageError(const AMessage: string);
procedure MessageWarning(const AMessage: string);
function MessageQuestion(const AMessage: string): Boolean;

// string processing
function AddColon(const Source: string): string;
function AddEndEllipsis(const Source: string): string;
function DropAmpersand(const Source: string): string;
function DropColon(const Source: string): string;
function DropEndEllipsis(const Source: string): string;
function DropT(const Source: string): string;
function dxValidatePath(const S: string): string;
function ReplaceSubStr(const Source, OldChars, NewChars: string): string;
function ReplicateChar(const S: string; ACount: Integer): string;

function dxBoolToStr(AValue: Boolean): string;
function dxSameStr(const S1, S2: string): Boolean;
function dxSameText(const S1, S2: string): Boolean;

function FormatFontInfo(AFont: TFont): string;
procedure FontInfoToText(AFont: TFont; AEdit: TcxTextEdit);

function DecodePageIndexes(const Source: string; out AOutput: TdxPSPageIndexes): Boolean;
function EncodePageIndexes(const ASource: TdxPSPageIndexes): string;

// Number metrics conversation
function Chars2Int(const AText: string; AnUpperCase: Boolean): Integer;
function Int2Chars(AValue: Integer; AnUpperCase: Boolean): string;
function Roman2Int(AText: string; AnUpperCase: Boolean): Integer;
function Int2Roman(AValue: Integer; AnUpperCase: Boolean): string;

//RTTI
function HasPropertyEx(AClass: TClass; const AName: string; ATypeKinds: TTypeKinds): Boolean; overload;
function HasPropertyEx(AnObject: TObject; const AName: string; ATypeKinds: TTypeKinds): Boolean; overload;
function HasProperty(AClass: TClass; const AName: string): Boolean; overload;
function HasProperty(AnObject: TObject; const AName: string): Boolean; overload;

function GetProperty(AnObject: TObject; const AName: string): Variant;
procedure SetProperty(AnObject: TObject; const AName: string; const AValue: Variant);

function dxGetStoringSectionName(AComponent: TComponent): string;
procedure dxDrawComboBoxItem(ACanvas: TCanvas; const R: TRect; const AText: string;
  AnImageList: TCustomImageList; AnImageIndex: Integer; AState: TOwnerDrawState);

procedure dxLoadStrings(AIniFile: TCustomIniFile; const ASectionName: string; AStrings: TStrings);
procedure dxSaveStrings(AIniFile: TCustomIniFile; const ASectionName: string; AStrings: TStrings);
procedure dxLoadListViewColumns(AIniFile: TCustomIniFile; ASectionName: string; AListView: TListView);
procedure dxSaveListViewColumns(AIniFile: TCustomIniFile; ASectionName: string; AListView: TListView);

procedure dxRestoreListViewSelection(AListView: TListView; ASelection: TList);
procedure dxSaveListViewSelection(AListView: TListView; ASelection: TList);

procedure CopyImages(ASourceHandle: HIMAGELIST; ADest: TCustomImageList);

procedure dxShiftIntegerListValues(AList: TList; AValue: Integer);

function dxCheckStateImageIndexMap(AState: TCheckBoxState): Integer;
procedure dxCreateCheckMarkImages(AnImageList: TcxImageList);
procedure dxSetupPreviewControlLookAndFeel(ALookAndFeel: TcxLookAndFeel;
  AKind: TcxLookAndFeelKind; AControl: TcxControl);

function dxPSDrawModeImages: TCustomImageList;

// VCL Helpers - introduced mostly because of Delphi.Net

function dxAllocatehWnd(AMethod: TWndMethod): HWND;
procedure dxDeallocatehWnd(AWnd: HWND);

function TTagToInt(AValue: TdxNativeInt): TdxNativeInt;
function TTagToObj(AValue: TdxNativeInt): TObject;
function TTagToClass(AValue: TdxNativeInt): TClass;

function MakeTTag(AValue: TdxNativeInt): TdxNativeInt; overload;
function MakeTTag(AValue: TObject): TdxNativeInt; overload;
function MakeTTag(AValue: TClass): TdxNativeInt; overload;

function Control_GetColor(AControl: TControl): TColor;
function Control_GetControlStyle(AControl: TControl): TControlStyle;
function Control_GetCtl3D(AControl: TWinControl): Boolean;
function Control_GetFont(AControl: TControl): TFont;
function Control_GetPopupMenu(AControl: TControl): TPopupMenu;
function Control_GetText(AControl: TControl): string;
procedure Control_SetParentBackground(AControl: TWinControl; Value: Boolean);

procedure Control_DoContextPopup(AControl: TControl; const Pt: TPoint; var AHandled: Boolean);
procedure Control_PaintWindow(AControl: TWinControl; DC: HDC);
procedure Control_SendCancelMode(AControl, ASender: TControl);
procedure Control_UpdateBoundsRect(AControl: TControl; const R: TRect); overload;
procedure Control_UpdateBoundsRect(AControl: TControl; ALeft, ARight, AWidth, AHeight: Integer); overload;

procedure PopupMenu_DoPopup(APopupMenu: TPopupMenu);

function RichEdit_GetBorderStyle(AControl: TCustomRichEdit): TBorderStyle;
function RichEdit_GetLines(AControl: TCustomRichEdit): TStrings;

procedure dxLoadBitmapFromResource(ABitmap: TBitmap; const AResName: string); overload;
procedure dxLoadBitmapFromResource(ASmartImage: TdxSmartImage; const AResName: string); overload;
procedure dxLoadIconFromResource(AIcon: TIcon; const AResName: string);
procedure dxLoadIconFromResourceEx(AImage: TImage; const AResName: string); overload;
procedure dxLoadIconFromResourceEx(AImage: TcxImage; const AResName: string); overload;
procedure dxLoadImageListFromResources(AImageList: TcxImageList; const AResourceName: string; AInstance: HINST = 0);

function IsDelphiObject(AData: TdxNativeUInt): Boolean;

implementation

uses
  Types, StrUtils, Consts, ActiveX, ShlObj, ShellAPI,
{$IFDEF USEJPEGIMAGE}
  Jpeg,
{$ENDIF}
{$IFNDEF DELPHI15}
  FileCtrl,
{$ENDIF}
  dxPSRes, dxPSImgs, cxShellControls, dxDPIAwareUtils;


const
  CharCount = 26;
  Chars: array[Boolean] of string = (('abcdefghijklmnopqrstuvwxyz'), ('ABCDEFGHIJKLMNOPQRSTUVWXYZ'));

  ColumnPath = '\ColumnWidths';               // Don't Localize
  ColumnPattern = 'Column%d';                 // Don't Localize

var
  FDrawModeImages: TCustomImageList;
  FPatternBrush: HBRUSH;
  FShellLargeImages: TCustomImageList;
  FShellSmallImages: TCustomImageList;
  FNonTrueTypeFonts: TStringList;
  FTrueTypeFonts: TStringList;

type

  TControlAccess = class(TControl);
  TCustomRichEditAccess = class(TCustomRichEdit);
  TGraphicAccess = class(TGraphic);
  TPopupMenuAccess = class(TPopupMenu);
  TWinControlAccess = class(TWinControl);
  TCustomFormAccess = class(TCustomForm);
  TcxControlAccess = class(TcxControl);

function dxPSFixInvalidFileNameChars(const S: string): string;
var
  I: Integer;
begin
  Result := S;
  for I := 1 to Length(InvalidFileNameChars) do
    Result := StringReplace(Result, InvalidFileNameChars[I], '_', [rfReplaceAll, rfIgnoreCase]);
end;

procedure ActivateComboBoxControl(AForm: TCustomForm; AComboBoxControl: TWinControl);
begin
  AForm.ActiveControl := AComboBoxControl;
  if AComboBoxControl is TcxCustomComboBox then
    TcxCustomComboBox(AComboBoxControl).DroppedDown := True;
end;

procedure CheckDialogFormHelpContext(AForm: TCustomForm; AHelpBtnHolder: TdxLayoutItem);
var
  ATempForm: TCustomFormAccess;
begin
  ATempForm := TCustomFormAccess(AForm);
  if ATempForm.HelpContext = 0 then
    ATempForm.BorderIcons := ATempForm.BorderIcons - [biHelp]
  else
    ATempForm.BorderIcons := ATempForm.BorderIcons + [biHelp];

  AHelpBtnHolder.Visible := ATempForm.HelpContext <> 0;
end;

function FontStyleNames(AFontStyle: TFontStyle): string;
begin
  case AFontStyle of
    fsBold:
      Result := cxGetResourceString(@sdxFontStyleBold);
    fsItalic:
      Result := cxGetResourceString(@sdxFontStyleItalic);
    fsUnderline:
      Result := cxGetResourceString(@sdxFontStyleUnderline);
  else
    Result := cxGetResourceString(@sdxFontStyleStrikeOut);
  end;
end;

function NonTrueTypeFonts: TStringList;
begin
  if FNonTrueTypeFonts = nil then
  begin
    FNonTrueTypeFonts := TStringList.Create;
    FNonTrueTypeFonts.Sorted := True;
  end;
  Result := FNonTrueTypeFonts;
end;

function TrueTypeFonts: TStringList;
begin
  if FTrueTypeFonts = nil then
  begin
    FTrueTypeFonts := TStringList.Create;
    FTrueTypeFonts.Sorted := True;
  end;
  Result := FTrueTypeFonts;
end;

function MinMax(A, B, C: Integer): Integer;
begin
  if B > C then
    Result := A
  else
    if A < B then
      Result := B
    else
      if A > C then
        Result := C
      else
        Result := A;
end;

function SetLoWord(AValue: Integer; ALoWord: Word): Integer;
begin
  Result := (AValue and Integer($0000FFFF)) or (ALoWord shl 16);
end;

function SetHiWord(AValue: Integer; AHiWord: Word): Integer;
begin
  Result := (AValue and Integer($FFFF0000)) or (AHiWord and $FFFF);
end;

function SetLoHiWords(AValue: Integer; ALoWord, AHiWord: Word): Integer;
begin
  Result := SetHiWord(SetLoWord(AValue, ALoWord), AHiWord);
end;

function ScalePoint(const Pt: TPoint; Numerator, Denominator: Integer): TPoint;
begin
  Result.X := MulDiv(Pt.X, Numerator, Denominator);
  Result.Y := MulDiv(Pt.Y, Numerator, Denominator);
end;

function ArePointsEqual(const Pt1, Pt2: TPoint): Boolean;
begin
  Result := (Pt1.X = Pt2.X) and (Pt1.Y = Pt2.Y);
end;

function GetRgnData(ARgn: HRGN; out ARgnDataHeader: TRgnDataHeader;
  out ARects: TRects): Integer;
var
  Size: Integer;
  RgnData: PRgnData;
  I: Integer;
begin
  Size := GetRegionData(ARgn, 0, nil);
  RgnData := AllocMem(SizeOf(TRgnDataHeader) + SizeOf(TRect) * (Size - SizeOf(TRgnDataHeader)));
  try
    GetRegionData(ARgn, Size, RgnData);
    ARgnDataHeader := RgnData.rdh;
    Result := RgnData.rdh.nCount;
    SetLength(ARects, Result);
    for I := 0 to Result - 1 do
      Move(RgnData.Buffer[I * SizeOf(TRect)], ARects[I], SizeOf(TRect));
  finally
    FreeMem(RgnData, Size);
  end;
end;

function ExcludeClipRect(DC: HDC; const R: TRect): HRGN;
begin
  with R do
    Result := ExcludeClipRect(DC, Left, Top, Right, Bottom);
end;

function ExcludeClipRect(DC: HDC; ALeft, ATop, ARight, ABottom: Integer): HRGN;
begin
  Result := Windows.CreateRectRgn(0, 0, 0, 0);
  if Windows.GetClipRgn(DC, Result) <> 1 then
  begin
    Windows.DeleteObject(Result);
    Result := 0;
  end;
  Windows.ExcludeClipRect(DC, ALeft, ATop, ARight, ABottom);
end;

function IntersectClipRect(DC: HDC; const R: TRect; AlwaysClip: Boolean = False): HRGN;
begin
  with R do
    Result := IntersectClipRect(DC, Left, Top, Right, Bottom);
end;

function IntersectClipRect(DC: HDC; ALeft, ATop, ARight, ABottom: Integer; AlwaysClip: Boolean = False): HRGN;
begin
  Result := Windows.CreateRectRgn(0, 0, 0, 0);
  if Windows.GetClipRgn(DC, Result) <> 1 then
  begin
    Windows.DeleteObject(Result);
    Result := 0;
  end
  else
    if IsMetaFileDC(DC) then
    begin
      Result := Windows.CreateRectRgn(0, 0, 320000, 320000);
      if Windows.SelectClipRgn(DC, Result) = ERROR then
      begin
        DeleteObject(Result);
        Result := 0;
        Exit;
      end;
    end;
  Windows.IntersectClipRect(DC, ALeft, ATop, ARight, ABottom);
end;

function RectVisible(DC: HDC; const ARect: TRect): Boolean;
begin
  if IsMetafileDC(DC) then
    Result := True
  else
    Result := Windows.RectVisible(DC, ARect);
end;

procedure RestoreClipRgn(DC: HDC; var ARgn: HRGN);
begin
  Windows.SelectClipRgn(DC, ARgn);
  if ARgn <> 0 then Windows.DeleteObject(ARgn);
  ARgn := 0;
end;

function dxBoolToStr(AValue: Boolean): string;
begin
  Result := SysUtils.BoolToStr(AValue, True);
end;

function dxSameStr(const S1, S2: string): Boolean;
begin
  Result := AnsiSameStr(S1, S2);
end;

function dxSameText(const S1, S2: string): Boolean;
begin
  Result := AnsiSameText(S1, S2);
end;

function dxAreBitmapsEqual(ABitmap1, ABitmap2: TBitmap): Boolean;
begin
  Result := dxAreGraphicsEqual(ABitmap1, ABitmap2);
end;

function dxAreBrushesEqual(ABrush1, ABrush2: TBrush): Boolean;
begin
  Result := ((ABrush1 = nil) and (ABrush2 = nil)) or
    ((ABrush1 <> nil) and (ABrush2 <> nil) and
     (ABrush1.Color = ABrush2.Color) and
     (ABrush1.Style = ABrush2.Style));
end;

function dxAreFontsEqual(AFont1, AFont2: TFont): Boolean;
begin
  if (AFont1 = nil) or (AFont2 = nil) then
    Result := AFont1 = AFont2
  else
    Result :=
     (AFont1.Orientation = AFont2.Orientation) and
     (AFont1.Color = AFont2.Color) and
     (AFont1.Name = AFont2.Name) and
     (AFont1.Pitch = AFont2.Pitch) and
     (AFont1.Style = AFont2.Style) and
     (AFont1.Height = AFont2.Height);
end;

function dxAreFontsEqual(const AFont1Name: string; AFont1Color: TColor;
  AFont1Pitch: TFontPitch; AFont1Style: TFontStyles;
  AFont1Height: Integer; AFont2: TFont): Boolean;
begin
  Result :=
    (AFont2 <> nil) and
    (AFont1Color = AFont2.Color) and
    (AFont1Height = Abs(AFont2.Height)) and
    (AFont1Pitch = AFont2.Pitch) and
    (AFont1Style = AFont2.Style) and
    (AFont1Name = AFont2.Name);
end;

function dxAreGraphicsEqual(AGraphic1, AGraphic2: TGraphic): Boolean;
begin
  Result := (AGraphic1 = AGraphic2) or ((AGraphic1 <> nil) and
    TGraphicAccess(AGraphic1).Equals(AGraphic2));
end;

function dxArePensEqual(APen1, APen2: TPen): Boolean;
begin
  Result := ((APen1 = nil) and (APen2 = nil)) or
    ((APen1 <> nil) and (APen2 <> nil) and
     (APen1.Color = APen2.Color) and
     (APen1.Mode = APen2.Mode) and
     (APen1.Style = APen2.Style) and
     (APen1.Width = APen2.Width));
end;

function FormatFontInfo(AFont: TFont): string;
var
  S: string;
  FS: TFontStyle;
begin
  Result := '';
  if AFont = nil then Exit;

  Result := Format('%d %s %s ', [AFont.Size, cxGetResourceString(@sdxPt), AFont.Name]);
  if AFont.Style <> [] then
  begin
    Result := Result + ' [';
    S := '';
    for FS := Low(TFontStyle) to High(TFontStyle) do
      if FS in AFont.Style then
      begin
        if S <> '' then
          S := S + ', ';
        S := S + FontStyleNames(FS);
      end;
    Result := Result + S + ']';
  end;
end;

procedure FontInfoToText(AFont: TFont; AEdit: TcxTextEdit);
begin
  AEdit.Text := FormatFontInfo(AFont);
  if ColorToRGB(AFont.Color) <> ColorToRGB(AEdit.Style.Color) then
    AEdit.Style.Font.Color := AFont.Color
  else
    AEdit.Style.Font.Color := clWindowText;
end;

type
  PSearchBuffer = ^TSearchBuffer;
  TSearchBuffer = record
    FontName: string;
    IsTrueType: Boolean;
  end;

function EnumFontsProc(var AnEnumLogFont: TEnumLogFont; var ATextMetric: TNewTextMetric;
  AFontType: Integer; AData: LPARAM): Integer; stdcall;
begin
  with AnEnumLogFont.elfLogFont do
  begin
    Result := Integer(not (StrIComp(PChar(PSearchBuffer(AData)^.FontName), PChar(@lfFaceName[0])) = 0));
    if Result = 0 then
      PSearchBuffer(AData)^.IsTrueType := AFontType and TRUETYPE_FONTTYPE = TRUETYPE_FONTTYPE;
  end;
end;

function dxIsTrueTypeFont(AFont: TFont): Boolean;

  function CalculateIsTrueTypeFont(AFont: TFont): Boolean;
  var
    DC: HDC;
    SearchBuffer: PSearchBuffer;
  begin
    DC := GetDC(0);
    try
      try
        New(SearchBuffer);
        try
          SearchBuffer^.FontName := AFont.Name;
          EnumFontFamilies(DC, nil, @EnumFontsProc, LPARAM(SearchBuffer));
          Result := SearchBuffer^.IsTrueType;
        finally
          Finalize(SearchBuffer^.FontName);
          Dispose(PSearchBuffer(SearchBuffer));
        end;
      except
        Result := False;
      end;
    finally
      ReleaseDC(0, DC);
    end;
  end;

begin
  Result := TrueTypeFonts.IndexOf(AFont.Name) <> -1; // found in TT Fonts
  if not Result then
  begin
    Result := NonTrueTypeFonts.IndexOf(AFont.Name) = -1; // not found in non TT Fonts
    if Result then
    begin
      Result := CalculateIsTrueTypeFont(AFont);
      if Result then
        TrueTypeFonts.Add(AFont.Name)
      else
        NonTrueTypeFonts.Add(AFont.Name);
    end;
  end;
end;

function IsDisplayDC(DC: HDC): Boolean;
begin
  Result := GetDeviceCaps(DC, TECHNOLOGY) = DT_RASDISPLAY;
end;

function IsMetafileDC(DC: HDC): Boolean;
begin
  Result := GetObjectType(DC) in [OBJ_METADC, OBJ_ENHMETADC];
end;

function IsPrinterDC(DC: HDC): Boolean;
begin
  Result := GetDeviceCaps(DC, TECHNOLOGY) = DT_RASPRINTER;
end;

function CreatePatternBrush: HBRUSH;
var
  PatternBitmap: HBITMAP;
  DC: HDC;
  X, Y: Integer;
begin
  PatternBitmap := CreateBitmap(8, 8, 1, 1, nil);
  try
    DC := CreateCompatibleDC(0);
    PatternBitmap := SelectObject(DC, PatternBitmap);
    for X := 0 to 7 do
      for Y := 0 to 7 do
        SetPixel(DC, X, Y, $FFFFFF * Byte(Odd(X) = Odd(Y)));
    PatternBitmap := SelectObject(DC, PatternBitmap);
    DeleteDC(DC);

    Result := Windows.CreatePatternBrush(PatternBitmap);
  finally
    DeleteObject(PatternBitmap);
  end;
end;

function PatternBrush: HBRUSH;
begin
  if FPatternBrush = 0 then
    FPatternBrush := CreatePatternBrush;
  Result := FPatternBrush;
end;

procedure ForcePictureToBitmap(APicture: TPicture);
var
  B: TBitmap;
begin
  with APicture do
    if (Graphic <> nil) and not Graphic.InheritsFrom(TBitmap) then
      if not Graphic.InheritsFrom(TIcon) then
      begin
        B := TBitmap.Create;
        try
          B.Assign(Graphic);
          Bitmap := B;
        finally
          B.Free;
        end;
      end
      else
        Bitmap := IconToBitmap(TIcon(Graphic));
end;

function CreateGraphic(AGraphicClass: TGraphicClass): TGraphic;
begin
  Result := AGraphicClass.Create;
end;

function IconToBitmap(AnIcon: TIcon): TBitmap;
//var
//  IconInfo: TIconInfo;
begin
  Result := TBitmap.Create;
  with Result do
  begin
    Height := AnIcon.Height;
    Width := AnIcon.Width;
    //GetIconInfo(AnIcon.Handle, IconInfo);
    //MaskHandle := IconInfo.hbmMask;
    Transparent := True;
    Canvas.Draw(0, 0, AnIcon);
  end;
end;

function CreateArrowBitmap(AUpDownGlyph: TdxUpDownGlyph; AWidth: Integer = 16;
  AHeight: Integer = 16; AFontSize: Integer = 12): TBitmap;
const
  GlyphIndexes: array[TdxUpDownGlyph] of Integer = (GLYPH_UPARROW, GLYPH_DOWNARROW);
begin
  Result := CreateGlyphBitmap(GlyphIndexes[AUpDownGlyph], AWidth, AHeight, AFontSize);
end;

function CreateDoubleArrowBitmap(AUpDownGlyph: TdxUpDownGlyph; AWidth: Integer  = 16;
  AHeight: Integer = 16; AFontSize: Integer = 8): TBitmap;
const
  GlyphIndexes: array[TdxUpDownGlyph] of Integer = (GLYPH_UPARROW, GLYPH_DOWNARROW);
  OffsetUp = 2;
  OffsetDown = -1;
var
  B1, B2: TBitmap;
begin
  B1 := CreateGlyphBitmap(GlyphIndexes[AUpDownGlyph], AWidth, AHeight, AFontSize);
  try
    B1.Transparent := True;
    B2 := CreateGlyphBitmap(GlyphIndexes[AUpDownGlyph], AWidth, AHeight, AFontSize);
    try
      B2.Transparent := True;
      Result := TBitmap.Create;
      with Result do
      begin
        Width := AWidth;
        Height := AHeight;
        Canvas.Brush.Color := clBtnFace;
        Canvas.FillRect(Rect(0, 0, AWidth, AHeight));
        Canvas.Draw(0, OffsetUp, B1);
        Canvas.Draw(0, OffsetDown, B2);
      end;
    finally
      B2.Free;
    end;
  finally
    B1.Free;
  end;
end;

function CreateGlyphBitmap(AGlyphIndex: Integer; AWidth: Integer = 16;
  AHeight: Integer = 16; AFontSize: Integer = 12): TBitmap;
var
  W, H, X, Y: Integer;
  Ch: Char;
begin
  W := AWidth;
  H := AHeight;
  if W = 0 then W := 16;
  if H = 0 then H := 16;

  Result := TBitmap.Create;
  with Result do
  begin
    Width := W;
    Height := H;

    Canvas.Brush.Color := clBtnFace;
    Canvas.FillRect(Rect(0, 0, Width, Height));

    Canvas.Font.Name := 'Marlett';
    Canvas.Font.Size := AFontSize;
    Canvas.Font.Color := clWindowText;
    Canvas.Font.Charset := SYMBOL_CHARSET;
    Ch := Chr(AGlyphIndex);
    X := 0;
    Y := 0;
    if (AWidth <> 0) and (AHeight <> 0) then
    begin
      X := (Width - Canvas.TextWidth(Ch)) div 2;
      Y := (Height - Canvas.TextHeight(Ch)) div 2;
    end
    else
    begin
      Width := Canvas.TextWidth(Ch);
      Height := Canvas.TextHeight(Ch);
    end;

    Canvas.TextOut(X, Y, Ch);
  end;
end;

procedure DrawBlendedText(ACanvas: TCanvas; const R: TRect; const AText: string; AFont: TFont);
var
  MemDC: HDC;
  MemBitmap: HBITMAP;

  procedure PrepareOffScreenStructures(DC: HDC; ASize: TSize);
  begin
    MemBitmap := CreateCompatibleBitmap(DC, ASize.cX, ASize.cY);
    MemDC := CreateCompatibleDC(DC);
    MemBitmap := SelectObject(MemDC, MemBitmap);
  end;

  procedure CreateTextOutClipPath(const R: TRect; const AText: string; AFont: TFont);
  var
    F: HFONT;
    C: TColor;
    BkMode: Integer;
  begin
    FillRect(MemDC, R, GetStockObject(WHITE_BRUSH));

    F := SelectObject(MemDC, AFont.Handle);
    C := SetTextColor(MemDC, $FFFFFF);
    BkMode := SetBkMode(MemDC, Windows.TRANSPARENT);

    BeginPath(MemDC);
      TextOut(MemDC, 0, 0, PChar(AText), Length(AText));
    EndPath(MemDC);

    SetBkMode(MemDC, BkMode);
    SetTextColor(MemDC, C);
    SelectObject(MemDC, F);

    SelectClipPath(MemDC, RGN_COPY);
  end;

  procedure PatternTextOutClipPath(const R: TRect; ATextColor: TColor);
  var
    BkColor: COLORREF;
  begin
    ATextColor := SetTextColor(MemDC, ATextColor);
    BkColor := SetBkColor(MemDC, $FFFFFF);
    FillRect(MemDC, R, PatternBrush);
    SetTextColor(MemDC, ATextColor);
    SetBkColor(MemDC, BkColor);
  end;

  procedure UnprepareOffScreenStructures;
  begin
    MemBitmap := SelectObject(MemDC, MemBitmap);
    DeleteDC(MemDC);
    DeleteObject(MemBitmap);
  end;

var
  ADC: HDC;
  ASize: TSize;
  AMemRect: TRect;
  F: HFONT;
  AX, AY: Integer;
begin
  ADC := ACanvas.Handle;
  F := SelectObject(ADC, AFont.Handle);
  GetTextExtentPoint(ADC, PChar(AText), Length(AText), ASize);

  AMemRect := Rect(0, 0, ASize.cX, ASize.cY);

  PrepareOffScreenStructures(ADC, ASize);
  try
    CreateTextOutClipPath(AMemRect, AText, AFont);
    PatternTextOutClipPath(AMemRect, ColorToRGB(AFont.Color));
    SelectClipRgn(MemDC, 0);

    with R do
    begin
      AX := Left + (Right - Left - ASize.cX) div 2;
      AY := Top + (Bottom - Top - ASize.cY) div 2;
    end;
    BitBlt(ADC, AX, AY, ASize.cX, ASize.cY, MemDC, 0, 0, SRCAND);
  finally
    UnprepareOffScreenStructures;
  end;
  SelectObject(ADC, F);
end;

procedure DrawGlyph(DC: HDC; const R: TRect; AGlyph: Byte; ACenter: Boolean = False);
var
  AChar: Char;
  APoint: TPoint;
  ASize: TSize;
begin
  AChar := Chr(AGlyph);
  APoint := R.TopLeft;
  if ACenter then
  begin
    GetTextExtentPoint32(DC, @AChar, 1, ASize);
    APoint.X := R.Left + (R.Right - R.Left - ASize.cX) div 2;
    APoint.Y := R.Top  + (R.Bottom - R.Top - ASize.cY) div 2;
  end;
  ExtTextOut(DC, APoint.X, APoint.Y, 0, @R, @AChar, 1, nil);
end;

procedure DrawSizeGrip(DC: HDC; R: TRect);
var
  V: Integer;
begin
  V := GetSystemMetrics(SM_CXVSCROLL);
  R := Rect(R.Right - V, R.Bottom - V, R.Right, R.Bottom);
  DrawFrameControl(DC, R, DFC_SCROLL, DFCS_SCROLLSIZEGRIP);
end;

procedure TransparentDraw(DrawDC: HDC; Brush: HBRUSH; const R: TRect;
  ABitmap: TBitmap);
const
  ROP_DSPDxax = $00E20746;
var
  BW, BH: Integer;
  DC, MaskDC: HDC;
  B, MaskHandle: HBITMAP;
  ATextColor, ABackColor: COLORREF;
  ABrush: HBRUSH;
begin
  with R do
  begin
    BW := ABitmap.Width;
    BH := ABitmap.Height;

    DC := CreateCompatibleDC(DrawDC);
    B := SelectObject(DC, CreateCompatibleBitmap(DrawDC, BW, BH));
    try
      BitBlt(DC, 0, 0, BW, BH, ABitmap.Canvas.Handle, 0, 0, SRCCOPY);

      MaskDC := CreateCompatibleDC(DrawDC);
      MaskHandle := SelectObject(MaskDC, CreateBitmap(BW, BH, 1, 1, nil));
      try
        ABackColor := SetBkColor(DC, ColorToRGB(ABitmap.TransparentColor){GetPixel(DC, 0, BH - 1)});
        BitBlt(MaskDC, 0, 0, BW, BH, DC, 0, 0, SRCCOPY);
        SetBkColor(DC, ABackColor);

        ATextColor := SetTextColor(DC, 0);
        ABackColor := SetBkColor(DC, $FFFFFF);
        ABrush := SelectObject(DC, Brush);
        BitBlt(DC, 0, 0, BW, BH, MaskDC, 0, 0, ROP_DSPDxax);
        SelectObject(DC, ABrush);
        SetTextColor(DC, ATextColor);
        SetBkColor(DC, ABackColor);
      finally
        DeleteObject(SelectObject(MaskDC, MaskHandle));
        DeleteDC(MaskDC);
      end;

      BitBlt(DrawDC, Left, Top, Right - Left, Bottom - Top, DC, 0, 0, SRCCOPY);
    finally
      DeleteObject(SelectObject(DC, B));
      DeleteDC(DC);
    end;
  end;
end;

{.$WARN SYMBOL_DEPRECATED OFF}

function CopyDeviceMode(Src: HGLOBAL): HGLOBAL;
var
  Size: Integer;
  SrcPtr, DestPtr: PChar;
begin
  if Src <> 0 then
  begin
    Size := GlobalSize(Src);
    Result := GlobalAlloc(GHND, Size);
    if Result <> 0 then
    begin
      SrcPtr := GlobalLock(Src);
      if SrcPtr <> nil then
      try
        DestPtr := GlobalLock(Result);
        if DestPtr <> nil then
        try
          Move(SrcPtr^, DestPtr^, Size);
        finally
          GlobalUnlock(Result);
        end;
      finally
        GlobalUnlock(Src);
      end;
    end;
  end
  else
    Result := 0;
end;

{.$WARN SYMBOL_DEPRECATED ON}

function GetDesktopWorkArea: TRect;

  function IntersectionArea(const ABounds: TRect; APoint: TPoint; var Area: TRect): Integer;
  var
    R: TRect;
  begin
    Area := cxControls.GetDesktopWorkArea(APoint);
    if IntersectRect(R, ABounds, Area) then
      Result := Abs(R.Right - R.Left) * Abs(R.Bottom - R.Top)
    else
      Result := 0;
  end;

var
  R, R1: TRect;
begin
  if Application.MainForm <> nil then
  begin
    Result := Application.MainForm.BoundsRect;
    if IntersectionArea(Result, Result.TopLeft, R) >
      IntersectionArea(Result, Result.BottomRight, R1) then
      Result := R
    else
      Result := R1;
  end
  else
    Result := GetMonitorWorkArea(0);
end;

function GetLongFileName(const Source: string): string;
var
  Handle: THandle;
  Proc: function(ShortPathName, LongPathName: PChar; cchBuffer: Integer): Integer; stdcall;
  Buffer: array[0..MAX_PATH] of Char;
begin
  Handle := GetModuleHandle(kernel32);
  if Handle <> 0 then
  begin
    @Proc := GetProcAddress(Handle, 'GetLongPathNameW');
    if (@Proc <> nil) and (Proc(PChar(Source), Buffer, SizeOf(Buffer)) <> 0) then
      Result := Buffer
    else
      Result := Source;
  end
  else
    Result := Source;
end;

function GetMachineName: string;
var
  BufferSize: DWORD;
  Buffer:  array[0..MAX_COMPUTERNAME_LENGTH + 1] of Char ;
begin
  BufferSize := MAX_COMPUTERNAME_LENGTH + 1;
  GetComputerName(Buffer, BufferSize);
  Result := Buffer;
end;

function GetVolumeName(const APath: string): string;
var
  PrevErrorMode: UINT;
  Buffer: array[Byte] of Char;
  L, Flags: DWORD;
begin
  PrevErrorMode := Windows.SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    if GetVolumeInformation(PChar(APath), @Buffer, SizeOf(Buffer), nil, L, Flags, nil, 0) then
      Result := Buffer
    else
      Result := '';
  finally
    Windows.SetErrorMode(PrevErrorMode);
  end;
end;

function IsIntelliMousePresent: Boolean;
begin
  Result := Boolean(GetSystemMetrics(SM_MOUSEWHEELPRESENT));
end;

function IsNetworkPresent: Boolean;
begin
  Result := GetSystemMetrics(SM_NETWORK) and $01 = $01;
end;

function IsVolume(const APath: string): Boolean;
begin
  Result := ExtractFileDir(APath) = APath;
end;

function PopulateShellImages(FullInit: Boolean): Boolean;
var
  ShellDLLLoaded: Boolean;
  ShellDLL: HMODULE;
  Proc: function(FullInit: BOOL): BOOL; stdcall;
begin
  Result := False;
  if IsWinNT then
  begin
    ShellDLLLoaded := False;
    ShellDLL := GetModuleHandle(ShellAPI.Shell32);
    if ShellDLL = 0 then
    begin
      ShellDLL := LoadLibrary(ShellAPI.Shell32);
      if ShellDLL <= Windows.HINSTANCE_ERROR then
        ShellDLL := 0;
      ShellDLLLoaded := ShellDLL <> 0;
    end;
    if ShellDLL <> 0 then
    try
      Proc := GetProcAddress(ShellDll, PChar(660));
      Result := (@Proc <> nil) and Proc(FullInit);
    finally
      if ShellDLLLoaded then FreeLibrary(ShellDll);
    end;
  end;
end;

function ShellLargeImages: TCustomImageList;
begin
  if FShellLargeImages = nil then
    FShellLargeImages := TcxShellImageList.Create(SHGFI_LARGEICON);
  Result := FShellLargeImages;
end;

function ShellSmallImages: TCustomImageList;
begin
  if FShellSmallImages = nil then
    FShellSmallImages := TcxShellImageList.Create(SHGFI_SMALLICON);
  Result := FShellSmallImages;
end;

function BFFCallBack(Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer; stdcall;
var
  Path: PChar absolute lParam;
  B: Boolean;
  Buffer: array[0..MAX_PATH] of Char;
  S: string;
begin
  Result := 0;
  if uMsg = BFFM_INITIALIZED then
  begin
    B := not IsWin9X or (IsVolume(Path) and DirectoryExists(Path));
    if B then
      SendMessage(Wnd, BFFM_SETSELECTION, WPARAM(True), lpData);
  end;

  if uMsg = BFFM_SELCHANGED then
  begin
    B := SHGetPathFromIDList(PItemIDList(lParam), @Buffer);
    if B then
    begin
      SetString(S, PChar(@Buffer), MAX_PATH);
      B := DirectoryExists(S);
    end;
    SendMessage(Wnd, BFFM_ENABLEOK, 0, Ord(B));
  end;

  if (uMsg = BFFM_VALIDATEFAILEDA) or (uMsg = BFFM_VALIDATEFAILEDW) then
  begin
    S := Format(cxGetResourceString(@sdxInvalidRootDirectory), [Path]);
    Result := Ord(MessageQuestion(S));
    if Result = 1 then
      SendMessage(Wnd, BFFM_ENABLEOK, 0, 0);
  end;
end;

function ShowSystemSelectFolderDlg(var ADirPath: string): Boolean;
const
  EditBoxFlags: array[Boolean] of UINT = (0, BIF_EDITBOX);
  NewDialogStyleFlags: array[Boolean] of UINT = (0, BIF_NEWDIALOGSTYLE);
  ValidateFlags: array[Boolean] of UINT = (0, BIF_VALIDATE);
var
  ShellMalloc: IMalloc;
  Buffer: PChar;
  BrowseInfo: TBrowseInfo;
  PrevErrorMode: Cardinal;
  WindowList:  Pointer ;
  IDList:  PItemIDList ;
begin
  if (ShGetMalloc(ShellMalloc) = S_OK) and (ShellMalloc <> nil) then
  begin
    Buffer := ShellMalloc.Alloc(MAX_PATH);
    try
      FillChar(BrowseInfo, SizeOf(TBrowseInfo), 0);
      BrowseInfo.hwndOwner := Application.Handle;
      BrowseInfo.pszDisplayName := Buffer;
      BrowseInfo.lpszTitle := PChar(cxGetResourceString(@sdxSelectNewRoot));

      BrowseInfo.ulFlags := BIF_RETURNONLYFSDIRS or EditBoxFlags[IsComCtrlVersion471] or
        NewDialogStyleFlags[IsComCtrlVersion500] or ValidateFlags[IsComCtrlVersion471];
        if IsVolume(ADirPath) then
          ADirPath := ADirPath + '\';
          BrowseInfo.lpfn := BFFCallBack;
          BrowseInfo.lParam := LPARAM(PChar(ADirPath));

          WindowList := DisableTaskWindows(0);
          try
            PrevErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
            try
              IDList := SHBrowseForFolder(BrowseInfo);
              Result := IDList <> nil;
              if Result then
              begin
                SHGetPathFromIDList(IDList, Buffer);
                ShellMalloc.Free(IDList);
                if StrLen(Buffer) > 0 then ADirPath := Buffer;
              end;
            finally
              SetErrorMode(PrevErrorMode);
            end;
          finally
            EnableTaskWindows(WindowList);
          end;
    finally
      ShellMalloc.Free(Buffer);
    end;
  end
  else
    Result := False;
end;

function FormatFileSize(const AFileSize: Int64): string;
const
  FormatTemplate = '##0.#';
  KiloByte = 1024;
  MegaByte = KiloByte * KiloByte;
  GigaByte = KiloByte * MegaByte;
begin
  if AFileSize > GigaByte then
    Result := FormatFloat(FormatTemplate, AFileSize / GigaByte) + ' ' +
      cxGetResourceString(@sdxGigaBytes)
  else
    if AFileSize > MegaByte then
      Result := FormatFloat(FormatTemplate, AFileSize / MegaByte) + ' ' +
        cxGetResourceString(@sdxMegaBytes)
    else
      if AFileSize > KiloByte then
        Result := FormatFloat(FormatTemplate, AFileSize / KiloByte) + ' ' +
        cxGetResourceString(@sdxKiloBytes)
      else
        Result := FormatFloat(FormatTemplate, AFileSize) + ' ' +
          cxGetResourceString(@sdxBytes);
end;

function ValidateFileName(const FileName: string): Boolean;

  function HasChars(const Str, Substr: string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 1 to Length(Substr) do
      if Pos(Substr[I], Str) > 0 then
      begin
        Result := True;
        Break;
      end;
  end;

begin
  Result := (Trim(FileName) <> '') and not HasChars(FileName, '<>"[]|');
  if Result then
    Result := Pos('\', ExtractFileName(FileName)) = 0;
end;

procedure Delay(Value: DWORD); //milliseconds
var
  T: DWORD;
begin
  T := GetTickCount;
  while GetTickCount - T < Value do ;
end;

{ Strings management routines }

function dxValidatePath(const S: string): string;
begin
  if (S <> '') and (S[Length(S)] <> PathDelim) then
    Result := S + PathDelim
  else
    Result := S;
end;

function ReplaceSubStr(const Source, OldChars, NewChars: string): string;
var
  L, P: Integer;
begin
  Result := Source;
  P := Pos(OldChars, Result);
  if P > 0 then
  begin
    L := Length(OldChars);
    while P > 0 do
    begin
      Delete(Result, P, L);
      if NewChars <> #0 then
        Insert(NewChars, Result, P);
      P := Pos(OldChars, Result);
    end;
  end;
end;

function ReplicateChar(const S: string; ACount: Integer): string;
begin
  Result := DupeString(S, ACount);
end;

procedure SplitString(const ASource, ASeparator: string; AStrings: TStrings);
var
  P, L: Integer;
  Buffer: string;
begin
  Buffer := ASource;
  P := Pos(ASeparator, Buffer);
  if P > 0 then
  begin
    L := Length(ASeparator);
    while P > 0 do
    begin
      if P > 1 then
        AStrings.Add(Copy(Buffer, 1, P + L - 2));
      Delete(Buffer, 1, P + L - 1);
      P := Pos(ASeparator, Buffer);
    end;
  end;
  if Length(Buffer) > 0 then AStrings.Add(Buffer);
end;

function DecodePageIndexes(const Source: string; out AOutput: TdxPSPageIndexes): Boolean;

  function ProcessRange(const S: string; out AArray: TdxPSPageIndexes): Boolean;
  var
    V1, V2, P, Code1, Code2, I: Integer;
    SLeft, SRight: string;
  begin
    V1 := 0;
    V2 := 0;
    P := Pos(dxPSGlbl.cPageRangeSeparator, S);
    Result := P <> 0;
    if Result then
    begin
      SLeft := Copy(S, 1, P - 1);
      SRight := Copy(S, P + 1, Length(S) - P);
      Val(SLeft, V1, Code1);
      Val(SRight, V2, Code2);
      Result := (Code1 = 0) and (Code2 = 0) and (V2 >= V1);
   end;

   if Result then
   begin
     SetLength(AArray, V2 - V1 + 1);
     for I := V1 to V2 do
       AArray[I - V1] := I;
   end
   else
     SetLength(AArray, 0);
  end;

var
  Buffer: string;
  Strings: TStrings;
  S: string;
  I, V, Code, L, J: Integer;
  SubRange: TIntegers;
begin
  SetLength(AOutput, 0);
  try
    Buffer := StringReplace(Source, ' ', '', [rfReplaceAll, rfIgnoreCase]);
    if Length(Buffer) > 0 then
    begin
      Strings := TStringList.Create;
      try
        SplitString(Buffer, dxPSGlbl.cPageSeparator, Strings);
        for I := 0 to Strings.Count - 1 do
        begin
          S := Strings[I];
          Val(S, V, Code);
          if (Code = 0) and (V >= 0) then
          begin
            SetLength(AOutput, Length(AOutput) + 1);
            AOutput[Length(AOutput) - 1] := V;
          end
          else
            if ProcessRange(S, SubRange) then
            begin
              L := Length(AOutput);
              SetLength(AOutput, L + Length(SubRange));
              for J := L to L + Length(SubRange) - 1 do
                AOutput[J] := SubRange[J - L];
            end
            else
              Abort
        end;
      finally
        Strings.Free;
      end;
    end;
  except
    SetLength(AOutput, 0);
  end;
  Result := Length(AOutput) <> 0;
end;

function EncodePageIndexes(const ASource: TdxPSPageIndexes): string;
var
  AIsRange: Boolean;
  I: Integer;
begin
  Result := '';
  if Length(ASource) > 0 then
  begin
    AIsRange := False;
    Result := IntToStr(ASource[0]);
    for I := 1 to Length(ASource) - 1 do
    begin
      if ASource[I - 1] - 1 = ASource[I] then
        AIsRange := True
      else
        if not AIsRange then
          Result := Result + cPageSeparator + IntToStr(ASource[I])
        else
        begin
          AIsRange := False;
          Result := Result + cPageRangeSeparator + IntToStr(ASource[I]);
        end;
    end;
  end;
end;

function Int2Roman(AValue: Integer; AnUpperCase: Boolean): string;
const
  Max = 13;
  RomanNumbers: array[1..Max] of Integer =
    (1, 4, 5, 9, 10, 40, 50, 90, 100, 400, 500, 900, 1000);
  RomanStrings: array[Boolean, 1..Max] of string =
    (('i', 'iv', 'v', 'ix', 'x', 'xl', 'l', 'xc', 'c', 'cd', 'd', 'cm', 'm'),
     ('I', 'IV', 'V', 'IX', 'X', 'XL', 'L', 'XC', 'C', 'CD', 'D', 'CM', 'M'));
var
  Index: Integer;
begin
  Result := '';
  Index := Max;
  while AValue > 0 do
  begin
    while AValue < RomanNumbers[Index] do
      Dec(Index);
    while AValue >= RomanNumbers[Index] do
    begin
      Dec(AValue, RomanNumbers[Index]);
      Result := Result + RomanStrings[AnUpperCase, Index];
    end;
  end;
end;

function Roman2Int(AText: string; AnUpperCase: Boolean): Integer;
type
  TdxNumberOrder = (noOnes, noTens, noHundreds);
  TdxRomanNumber = 1..9;
const
  RomanNumbers: array[TdxNumberOrder, TdxRomanNumber] of Integer =
    ((  1,   2,   3,   4,   5,   6,   7,   8,   9),
     ( 10,  20,  30,  40,  50,  60,  70,  80,  90),
     (100, 200, 300, 400, 500, 600, 700, 800, 900));
  RomanThousand: array[Boolean] of string = ('m', 'M');
  RomanStrings: array[Boolean, TdxNumberOrder, TdxRomanNumber] of string =
    ((('i', 'ii', 'iii', 'iv', 'v', 'vi', 'vii', 'viii', 'ix'),
      ('x', 'xx', 'xxx', 'xl', 'l', 'lx', 'lxx', 'lxxx', 'lc'),
      ('c', 'cc', 'ccc', 'cd', 'd', 'dc', 'dcc', 'dccc', 'dm')),
     (('I', 'II', 'III', 'IV', 'V', 'VI', 'VII', 'VIII', 'IX'),
      ('X', 'XX', 'XXX', 'XL', 'L', 'LX', 'LXX', 'LXXX', 'XC'),
      ('C', 'CC', 'CCC', 'CD', 'D', 'DC', 'DCC', 'DCCC', 'CM')));
var
  Number: TdxRomanNumber;
  Order: TdxNumberOrder;
begin
  Result := 0;
  while (Length(AText) > 0) and (AText[1] = RomanThousand[AnUpperCase]) do
  begin
    Delete(AText, 1, 1);
    Inc(Result, 1000);
  end;
  if AText <> '' then
    for Order := noHundreds downto noOnes do
    begin
      Number := High(TdxRomanNumber);
      while (Number > 0) and (Pos(RomanStrings[AnUpperCase, Order, Number], AText) <> 1) do
        Dec(Number);
      if Number > 0 then
      begin
        Inc(Result, RomanNumbers[Order, Number]);
        Delete(AText, 1, Length(RomanStrings[AnUpperCase, Order, Number]));
        if AText = '' then Exit;
      end;
    end;
  if AText <> '' then Result := -1;
end;

function Chars2Int(const AText: string; AnUpperCase: Boolean): Integer;
begin
  if Length(AText) > 0 then
    Result := CharCount * (Length(AText) - 1) + Pos(AText[1], Chars[AnUpperCase])
  else
    Result := 0;
end;

function Int2Chars(AValue: Integer; AnUpperCase: Boolean): string;
var
  I, C: Integer;
begin
  I := AValue mod CharCount;
  if I = 0 then I := CharCount;
  C := AValue div CharCount;
  if I <> 0 then Inc(C);
  Result := ReplicateChar(Chars[AnUpperCase][I], C);
end;

function AddColon(const Source: string): string;
begin
  Result := Source;
  if Pos(':', Result) <> (Length(Result) - Length(':') + 1) then
    Result := Result + ':';
end;

function AddEndEllipsis(const Source: string): string;
begin
  Result := Source;
  if Pos('...', Result) <> (Length(Result) - Length('...') + 1) then
    Result := Result + '...';
end;

function DropAmpersand(const Source: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(Source) do
    if Source[I] <> '&' then
      Result := Result + Source[I];
end;

function DropColon(const Source: string): string;
begin
  Result := Source;
  if Result[Length(Result)] = ':' then
    Delete(Result, Length(Result), 1);
end;

function DropEndEllipsis(const Source: string): string;
begin
  Result := Source;
  while (Length(Result) > 0) and (Result[Length(Result)] = '.') do
    Delete(Result, Length(Result), 1);
end;

procedure MessageError(const AMessage: string);
begin
  MessageBeep(MB_ICONEXCLAMATION);
  MessageDlg(AMessage, mtError, [mbOK], 0);
end;

procedure MessageWarning(const AMessage: string);
begin
  MessageBeep(MB_ICONEXCLAMATION);
  MessageDlg(AMessage, mtWarning, [mbOK], 0);
end;

function MessageQuestion(const AMessage: string): Boolean;
begin
  MessageBeep(MB_ICONQUESTION);
  Result := MessageDlg(AMessage, mtConfirmation, [mbYes, mbNo], 0) = mrYes;
end;

function DropT(const Source: string): string;
begin
  Result := Source;
  if Result[1] = 'T' then Delete(Result, 1, 1);
end;

function HasPropertyEx(AClass: TClass; const AName: string; ATypeKinds: TTypeKinds): Boolean;
var
  PropList: PPropList;
  PropCount: Integer;
  I: Integer;
begin
  PropCount := GetPropList(AClass.ClassInfo, ATypeKinds, nil);
  if PropCount > 0 then
  begin
    PropList := AllocMem(PropCount * SizeOf(PPropInfo));
    try
      PropCount := GetPropList(AClass.ClassInfo, ATypeKinds, PropList);
      I := 0;
      while (I < PropCount) and (CompareText(dxShortStringToString(PropList^[I].Name), AName) <> 0) do
        Inc(I);
      Result := I < PropCount;
    finally
      FreeMem(PropList, PropCount * SizeOf(PPropInfo));
    end;
  end
  else
    Result := False;
end;

function HasPropertyEx(AnObject: TObject; const AName: string; ATypeKinds: TTypeKinds): Boolean;
begin
  Result := HasPropertyEx(AnObject.ClassType, AName, ATypeKinds);
end;

function HasProperty(AClass: TClass; const AName: string): Boolean;
begin
  Result := HasPropertyEx(AClass, AName, tkAny);
end;

function HasProperty(AnObject: TObject; const AName: string): Boolean;
begin
  Result := HasPropertyEx(AnObject, AName, tkAny);
end;

function GetProperty(AnObject: TObject; const AName: string): Variant;
begin
  if HasProperty(AnObject, AName) then
    Result := TypInfo.GetPropValue(AnObject, AName , False)
  else
    Result := Null;
end;

procedure SetProperty(AnObject: TObject; const AName: string; const AValue: Variant);
begin
  if HasProperty(AnObject, AName) then
    TypInfo.SetPropValue(AnObject, AName, AValue);
end;

function dxGetStoringSectionName(AComponent: TComponent): string;
begin
  Result := AComponent.Name;
  if Result = '' then
    Result := AComponent.ClassName + IntToStr(AComponent.ComponentIndex);
end;

procedure dxDrawComboBoxItem(ACanvas: TCanvas; const R: TRect; const AText: string;
  AnImageList: TCustomImageList; AnImageIndex: Integer; AState: TOwnerDrawState);
var
  X, Y: Integer;
begin
  ACanvas.FillRect(R);
  if IsImageAssigned(AnImageList, AnImageIndex) then
  begin
    with R do
    begin
      X := Left + 1;
      Y := Top + (Bottom - Top - AnImageList.Height) div 2;
    end;
    AnImageList.Draw(ACanvas, X, Y, AnImageIndex);
  end;

  with R do
  begin
    X := Left + 1 + Ord(IsImageAssigned(AnImageList, AnImageIndex)) * (AnImageList.Width + 2);
    Y := Top + (Bottom - Top - ACanvas.TextHeight(AText)) div 2;
  end;
  ACanvas.TextOut(X, Y, AText);
end;

procedure dxLoadStrings(AIniFile: TCustomIniFile; const ASectionName: string; AStrings: TStrings);
var
  AEntries: TStringList;
  I: Integer;
begin
  AStrings.BeginUpdate;
  try
    AStrings.Clear;
    AEntries := TStringList.Create;
    try
      AIniFile.ReadSection(ASectionName, AEntries);
      AStrings.Capacity := AEntries.Count;
      for I := 0 to AEntries.Count - 1 do
        AStrings.Add(AIniFile.ReadString(ASectionName, AEntries[I], ''));
    finally
      AEntries.Free;
    end;
  finally
    AStrings.EndUpdate;
  end;
end;

procedure dxSaveStrings(AIniFile: TCustomIniFile; const ASectionName: string; AStrings: TStrings);
var
  I: Integer;
begin
  AStrings.BeginUpdate;
  try
    AIniFile.EraseSection(ASectionName);
    for I := 0 to AStrings.Count - 1 do
      AIniFile.WriteString(ASectionName, IntToStr(I), AStrings[I]);
  finally
    AStrings.EndUpdate;
  end;
end;

procedure dxLoadListViewColumns(
  AIniFile: TCustomIniFile; ASectionName: string; AListView: TListView);
var
  I: Integer;
  S: string;
begin
  AListView.Columns.BeginUpdate;
  try
    ASectionName := dxValidatePath(ASectionName) + AListView.Name + ColumnPath;
    for I := 0 to AListView.Columns.Count - 1 do
    begin
      S := Format(ColumnPattern, [I]);
      if AIniFile.ValueExists(ASectionName, S) then
        AListView.Columns[I].Width := AIniFile.ReadInteger(ASectionName, S, 0);
    end;
  finally
    AListView.Columns.EndUpdate;
  end;
end;

procedure dxSaveListViewColumns(
  AIniFile: TCustomIniFile; ASectionName: string; AListView: TListView);
var
  I: Integer;
begin
  ASectionName := dxValidatePath(ASectionName) + AListView.Name + ColumnPath;
  AIniFile.EraseSection(ASectionName);
  for I := 0 to AListView.Columns.Count - 1 do
    AIniFile.WriteInteger(ASectionName, Format(ColumnPattern, [I]), AListView.Columns[I].Width);
end;

procedure dxLoadStringsFromRegistry(const APath: string; AStrings: TStrings);
begin
end;

procedure dxSaveStringsToRegistry(const APath: string; AStrings: TStrings);
begin
end;

procedure dxLoadListViewColumnsFromRegistry(AListView: TListView; const APath: string);
begin
end;

procedure dxSaveListViewColumnsToRegistry(AListView: TListView; const APath: string);
begin
end;

procedure dxSaveListViewSelection(AListView: TListView; ASelection: TList);
var
  I: Integer;
  Item: TListItem;
begin
  for I := 0 to AListView.Items.Count - 1 do
  begin
    Item := AListView.Items[I];
    if Item.Selected then ASelection.Add(Item.Data);
  end;
end;

procedure dxRestoreListViewSelection(AListView: TListView; ASelection: TList);
var
  I: Integer;
  Item: TListItem;
begin
  for I := 0 to ASelection.Count - 1 do
  begin
    Item := AListView.FindData(0, ASelection[I], True, True);
    if Item <> nil then
    begin
      Item.Selected := True;
      if I = 0 then
        Item.MakeVisible(True);
    end;
  end;
end;

procedure CopyImages(ASourceHandle: HIMAGELIST; ADest: TCustomImageList);
var
  ImageCount, ImageWidth, ImageHeight, I: Integer;
  Image, Mask: TBitmap;
  R: TRect;
begin
  ADest.Clear;
  ImageCount := ImageList_GetImageCount(ASourceHandle);
  if ImageCount = 0 then Exit;

  ImageList_GetIconSize(ASourceHandle, ImageWidth, ImageHeight);
  ADest.Width := ImageWidth;
  ADest.Height := ImageHeight;

  // we need to copy all color depth and alpha channel information
  ADest.Handle := ImageList_Duplicate(ASourceHandle);
  ADest.Clear;
  {???}
  R := Rect(0, 0, ImageWidth, ImageHeight);
  Image := TBitmap.Create;
  try
    Image.Height := ImageHeight;
    Image.Width := ImageWidth;
    Mask := TBitmap.Create;
    try
      Mask.Monochrome := True;
      Mask.Height := ImageHeight;
      Mask.Width := ImageWidth;

      for I := 0 to ImageCount - 1 do
      begin
        with Image.Canvas do
        begin
          FillRect(R);
          ImageList_Draw(ASourceHandle, I, Handle, 0, 0, ILD_NORMAL);
        end;
        with Mask.Canvas do
        begin
          FillRect(R);
          ImageList_Draw(ASourceHandle, I, Handle, 0, 0, ILD_MASK);
        end;
        ADest.Add(Image, Mask);
      end;
    finally
      Mask.Free;
    end;
  finally
    Image.Free;
  end;
end;

procedure dxShiftIntegerListValues(AList: TList; AValue: Integer);
var
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do
    AList.List[I] := Pointer(Integer(AList.List[I]) + AValue);
end;

function dxCheckStateImageIndexMap(AState: TCheckBoxState): Integer;
const
  CheckBoxStateMap: array[TCheckBoxState] of Integer = (0, 1, 2);
begin
  Result := 1 + CheckBoxStateMap[AState];
end;

procedure dxCreateCheckMarkImages(AnImageList: TcxImageList);
const
  MaskColor: TColor = clFuchsia;

  procedure DrawCheckBox(ACanvas: TCanvas; const R: TRect; State: TCheckBoxState);
  const
    InteriorColors: array[Boolean] of TColor = (clWindow, clBtnFace);
  var
    ASavedColor: TColor;
  begin
    ASavedColor := ACanvas.Font.Color;
    try
      ACanvas.Brush.Style := bsClear;

      //frame
      ACanvas.Font.Color := clBtnShadow;
      DrawGlyph(ACanvas.Handle, R, CheckBottomRightArcInnerIndex);
      DrawGlyph(ACanvas.Handle, R, CheckTopLeftArcInnerIndex);

      //interior
      ACanvas.Font.Color := InteriorColors[State = cbGrayed];
      DrawGlyph(ACanvas.Handle, R, CheckInteriorIndex);

      //checkmark
      if State in [cbChecked, cbGrayed] then
      begin
        ACanvas.Font.Color := clWindowText;
        DrawGlyph(ACanvas.Handle, R, CheckMarkIndex);
      end;
    finally
      ACanvas.Font.Color := ASavedColor;
    end;
  end;

var
  ABitmap: TcxBitmap;
  ACheckRect: TRect;
  ASize: TSize;
  AState: TCheckBoxState;
begin
  ABitmap := TcxBitmap.CreateSize(AnImageList.Width, AnImageList.Height, pf24bit);
  try
    ABitmap.cxCanvas.Font.Name := 'Marlett';
    ABitmap.cxCanvas.Font.Size := 10;
    ABitmap.cxCanvas.Font.Charset := SYMBOL_CHARSET;

    ABitmap.cxCanvas.FillRect(ABitmap.ClientRect, MaskColor);
    AnImageList.AddMasked(ABitmap, MaskColor);

    ACheckRect := ABitmap.ClientRect;
    ASize := ABitmap.cxCanvas.TextExtent(Chr(CheckBottomRightArcInnerIndex));
    InflateRect(ACheckRect, -(ABitmap.Width - ASize.cx) div 2, -(ABitmap.Height - ASize.cy) div 2);

    for AState := Low(TCheckBoxState) to High(TCheckBoxState) do
    begin
      ABitmap.cxCanvas.FillRect(ABitmap.ClientRect, MaskColor);
      DrawCheckBox(ABitmap.Canvas, ACheckRect, AState);
      AnImageList.AddMasked(ABitmap, MaskColor);
    end;
  finally
    ABitmap.Free;
  end;
end;

procedure dxSetupPreviewControlLookAndFeel(ALookAndFeel: TcxLookAndFeel;
  AKind: TcxLookAndFeelKind; AControl: TcxControl);
begin
  ALookAndFeel.Kind := AKind;
  ALookAndFeel.NativeStyle := False;
  if Assigned(AControl) and not TcxControlAccess(AControl).LookAndFeel.NativeStyle then
    ALookAndFeel.SkinName := TcxControlAccess(AControl).LookAndFeel.SkinName
  else
    ALookAndFeel.SkinName := '';
end;

function dxPSDrawModeImages: TCustomImageList;

  procedure LoadImages;

    procedure LoadImage(B: TBitmap; const AResName: string);
    begin
      dxLoadBitmapFromResource(B, AResName);
      FDrawModeImages.AddMasked(B, clDefault);
    end;

  var
    B: TBitmap;
  begin
    B := TBitmap.Create;
    try
      LoadImage(B, IDB_DXPSDRAWMODE_STRICT);
      LoadImage(B, IDB_DXPSDRAWMODE_ODDEVEN);
      LoadImage(B, IDB_DXPSDRAWMODE_CHESS);
      LoadImage(B, IDB_DXPSDRAWMODE_BORROW);
    finally
      B.Free;
    end;
  end;

begin
  if FDrawModeImages = nil then
  begin
    FDrawModeImages := TImageList.Create(nil);
    LoadImages;
  end;
  Result := FDrawModeImages;
end;

function dxAllocatehWnd(AMethod: TWndMethod): HWND;
begin
  Result := Classes.AllocatehWnd(AMethod);
end;

procedure dxDeallocatehWnd(AWnd: HWND);
begin
  if IsWindow(AWnd) then
    Classes.DeallocatehWnd(AWnd);
end;

function MakeBounds(ALeft, ATop, AWidth, AHeight: Integer): TRect;
begin
  with Result do
  begin
    Left := ALeft;
    Top := ATop;
    Right := ALeft + AWidth;
    Bottom := ATop + AHeight;
  end;
end;

function TTagToInt(AValue: TdxNativeInt): TdxNativeInt;
begin
  Result := AValue;
end;

function TTagToObj(AValue: TdxNativeInt): TObject;
begin
  Result := TObject(AValue);
end;

function TTagToClass(AValue: TdxNativeInt): TClass;
begin
  Result := TClass(AValue);
end;

function MakeTTag(AValue: TdxNativeInt): TdxNativeInt;
begin
  Result := AValue;
end;

function MakeTTag(AValue: TObject): TdxNativeInt;
begin
  Result := TdxNativeInt(AValue);
end;

function MakeTTag(AValue: TClass): TdxNativeInt;
begin
  Result := TdxNativeInt(AValue);
end;

function Control_GetColor(AControl: TControl): TColor;
begin
  Result := TControlAccess(AControl).Color;
end;

function Control_GetControlStyle(AControl: TControl): TControlStyle;
begin
  Result := TControlAccess(AControl).ControlStyle;
end;

function Control_GetCtl3D(AControl: TWinControl): Boolean;
begin
  Result := TWinControlAccess(AControl).Ctl3D;
end;

function Control_GetFont(AControl: TControl): TFont;
begin
  Result := TControlAccess(AControl).Font;
end;

function Control_GetPopupMenu(AControl: TControl): TPopupMenu;
var
  APopupComponent: TComponent;
begin
  if AControl is TcxControl then
    APopupComponent := TcxControlAccess(AControl).PopupMenu
  else
    APopupComponent := TControlAccess(AControl).PopupMenu;

  if APopupComponent is TPopupMenu then
    Result := TPopupMenu(APopupComponent)
  else
    Result := nil;
end;

function Control_GetText(AControl: TControl): string;
begin
  Result := TControlAccess(AControl).Text;
end;

procedure Control_SetParentBackground(AControl: TWinControl; Value: Boolean);
begin
  TWinControlAccess(AControl).ParentBackground := Value;
end;

procedure Control_DoContextPopup(AControl: TControl; const Pt: TPoint; var AHandled: Boolean);
begin
  AHandled := False;
  TControlAccess(AControl).DoContextPopup(Pt, AHandled);
end;

procedure Control_PaintWindow(AControl: TWinControl; DC: HDC);
begin
  TWinControlAccess(AControl).PaintWindow(DC);
end;

procedure Control_SendCancelMode(AControl, ASender: TControl);
begin
  TControlAccess(AControl).SendCancelMode(ASender);
end;

procedure Control_UpdateBoundsRect(AControl: TControl; const R: TRect);
begin
  TControlAccess(AControl).UpdateBoundsRect(R);
end;

procedure Control_UpdateBoundsRect(AControl: TControl; ALeft, ARight, AWidth, AHeight: Integer);
begin
  Control_UpdateBoundsRect(AControl, MakeBounds(ALeft, ARight, AWidth, AHeight));
end;

procedure PopupMenu_DoPopup(APopupMenu: TPopupMenu);
begin
  TPopupMenuAccess(APopupMenu).DoPopup(APopupMenu);
end;

function RichEdit_GetBorderStyle(AControl: TCustomRichEdit): TBorderStyle;
begin
  Result := TCustomRichEditAccess(AControl).BorderStyle;
end;

function RichEdit_GetLines(AControl: TCustomRichEdit): TStrings;
begin
  Result := TCustomRichEditAccess(AControl).Lines;
end;

procedure dxLoadImageListFromResources(
  AImageList: TcxImageList; const AResourceName: string; AInstance: HINST);
var
  ABitmap: TcxBitmap;
begin
  if AInstance = 0 then
    AInstance := HInstance;
  ABitmap := TcxBitmap.Create;
  try
    ABitmap.LoadFromResourceName(AInstance, AResourceName);
    AImageList.BeginUpdate;
    try
      AImageList.Clear;
      AImageList.Add(ABitmap, nil)
    finally
      AImageList.EndUpdate;
    end;
  finally
    ABitmap.Free;
  end;
end;

procedure dxLoadBitmapFromResource(ABitmap: TBitmap; const AResName: string);
begin
  ABitmap.LoadFromResourceName(hInstance, AResName);
end;

procedure dxLoadBitmapFromResource(ASmartImage: TdxSmartImage; const AResName: string); overload;
begin
  ASmartImage.LoadFromResource(hInstance, AResName, RT_BITMAP);
end;

procedure dxLoadIconFromResource(AIcon: TIcon; const AResName: string);
var
  ABitmap: TBitmap;
  AImageList: TcxImageList;
begin
  ABitmap := TBitmap.Create;
  try
    dxLoadBitmapFromResource(ABitmap, AResName);
    AImageList := TcxImageList.CreateSize(ABitmap.Width, ABitmap.Height);
    try
      AImageList.Add(ABitmap, nil);
      AIcon.Handle := ImageList_GetIcon(AImageList.Handle, 0, ILD_NORMAL);
    finally
      AImageList.Free;
    end;
  finally
    ABitmap.Free;
  end;
end;

procedure dxLoadIconFromResourceEx(AImage: TImage; const AResName: string);
var
  AIcon: TIcon;
begin
  AIcon := TIcon.Create;
  try
    dxLoadIconFromResource(AIcon, AResName);
    AImage.Picture.Graphic := AIcon;
  finally
    AIcon.Free;
  end;
end;

procedure dxLoadIconFromResourceEx(AImage: TcxImage; const AResName: string);
var
  AGlyph: TdxSmartGlyph;
begin
  AGlyph := TdxSmartGlyph.Create;
  try
    dxLoadBitmapFromResource(AGlyph, AResName);
    AGlyph.SourceDPI := dxDefaultDPI;
    AImage.Picture.Graphic := AGlyph;
  finally
    AGlyph.Free;
  end;
end;

function IsDelphiObject(AData: TdxNativeUInt): Boolean;
var
  P: Pointer;
  SelfPtr: Pointer;
begin
  Result := False;

  P := Pointer(AData);
  if IsBadReadPtr(P, SizeOf(Pointer)) then Exit;

  P := PPointer(P)^;
  if IsBadReadPtr(P, SizeOf(Pointer)) then Exit;

  SelfPtr := Pointer(TdxNativeInt(P) + vmtSelfPtr);
  if IsBadReadPtr(SelfPtr, SizeOf(Pointer)) then Exit;
  SelfPtr := PPointer(SelfPtr)^;

  Result := P = SelfPtr;
end;

procedure InitializeUnit;
begin
  PopulateShellImages(True);
end;

initialization
  CoInitialize(nil);
  dxUnitsLoader.AddUnit(@InitializeUnit, nil);

finalization
  FreeAndNil(FDrawModeImages);
  if FPatternBrush <> 0 then
    Windows.DeleteObject(FPatternBrush);

  FreeAndNil(FTrueTypeFonts);
  FreeAndNil(FNonTrueTypeFonts);
  FreeAndNil(FShellLargeImages);
  FreeAndNil(FShellSmallImages);

  CoUninitialize;

end.
