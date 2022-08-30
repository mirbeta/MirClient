{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxRichEdit.Platform.Win.FontCache;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Windows, Graphics, Classes, Forms, Generics.Defaults, Generics.Collections, SyncObjs,
  cxClasses, dxCoreClasses,

  dxRichEdit.Platform.Font,
  dxRichEdit.Platform.Win.Font,
  dxRichEdit.DocumentLayout.UnitConverter,
  dxRichEdit.Utils.UnicodeRangeInfo,
  dxRichEdit.Utils.Types;

type
  { TdxGdiFontCache }

  TdxTrueTypeFontInfo = class
  public const
    SymbolFontType = 256;
    FixedPitchFontType = 512;
  private
    FFontName: string;
    FFontType: Integer;
    FSupportedStylesCalculated: Boolean;
    FSupportedStyles: TdxSupportedFontStylesInfo;
    function GetSupportedFontStyles: TdxSupportedFontStylesInfo;
    function GetStylesInfo: TdxSupportedFontStylesInfo;
  public
    constructor Create(const AFontName: string; const AFontType: Integer);
    property FontName: string read FFontName;
    property FontType: Integer read FFontType;
    property StylesInfo: TdxSupportedFontStylesInfo read GetStylesInfo;
  end;

  TdxSystemTrueTypeFontDictionary = TObjectDictionary<string, TdxTrueTypeFontInfo>;

  TdxGdiFontCache = class(TdxFontCache)
  private type
  {$REGION 'type'}

    TTrueTypeFontLoader = class(TcxThread)
    private
      class function EnumFontInfoProc(var ALogFont: TLogFontW; ATextMetric: PTextMetricW;
        AFontType: Integer; AData: Pointer): Integer; stdcall; static;
      function CreateFontCharacterSet(ADC: HDC; const AFontName: string): TdxFontCharacterSet;
      function CanContinue: Boolean;
    protected
      procedure Execute; override;
    end;

    TCallbackData = record
      Thread: TTrueTypeFontLoader;
    end;
    PCallbackData = ^TCallbackData;

  {$ENDREGION}
  private class var
      FLoader: TTrueTypeFontLoader;
      FSystemTrueTypeFonts: TdxSystemTrueTypeFontDictionary;
      FUnicodeRangeInfo: TdxUnicodeRangeInfo;
  private
    class procedure Initialize;
    class procedure Finalize;
    function GetMeasurer: TdxGdiFontInfoMeasurer; inline;
  protected
    function CreateOverrideFontSubstitutes: TdxSystemTrueTypeFontDictionary; virtual;
    function CreateFontInfoMeasurer(AUnitConverter: TdxDocumentLayoutUnitConverter): TdxFontInfoMeasurer; override;
    function CreateFontInfoCore(const AFontName: string; ADoubleFontSize: Integer; const AFontStyle: TFontStyles): TdxFontInfo; override;
    function GetFontCharacterRanges(AFontInfo: TdxFontInfo): TdxFontCharacterRangeArray; override;

    class property UnicodeRangeInfo: TdxUnicodeRangeInfo read FUnicodeRangeInfo;
  public
    function CreateFontCharacterSet(const AFontName: string): TdxFontCharacterSet;
    function GetFontCharacterSet(const AFontName: string): TdxFontCharacterSet; override;
    function FindSubstituteFont(const ASourceFontName: string; ACharacter: Char; var AFontCharacterSet: TdxFontCharacterSet): string; override;
    function ShouldUseDefaultFontToDrawInvisibleCharacter(AFontInfo: TdxFontInfo; ACharacter: Char): Boolean; override;

    class function CreateSystemTrueTypeFonts: TStrings;
    class procedure PopulateNameToCharacterMap;
    class property SystemTrueTypeFonts: TdxSystemTrueTypeFontDictionary read FSystemTrueTypeFonts;

    property Measurer: TdxGdiFontInfoMeasurer read GetMeasurer;
  end;

implementation

uses
  SysUtils, dxCore, Character, ComObj, ActiveX;

const
  CLASS_CMultiLanguage: TGUID = '{275C23E2-3747-11D0-9FEA-00AA003F8646}';

type
  IMLangCodePages = interface(IUnknown)
    ['{359F3443-BD4A-11D0-B188-00AA0038C969}']
    function GetCharCodePages(const chSrc: Char; out pdwCodePages: DWORD): HResult; stdcall;
    function GetStrCodePages(const pszSrc: PChar; const cchSrc: ULONG; dwPriorityCodePages: DWORD; out pdwCodePages: DWORD; out pcchCodePages: ULONG): HResult; stdcall;
    function CodePageToCodePages(const uCodePage: SYSUINT; out pdwCodePages: LongWord): HResult; stdcall;
    function CodePagesToCodePage(const dwCodePages: LongWord; const uDefaultCodePage: SYSUINT; out puCodePage: SYSUINT): HResult; stdcall;
  end;

  IMLangFontLink = interface(IMLangCodePages)
    ['{359F3441-BD4A-11D0-B188-00AA0038C969}']
    function GetFontCodePages(const hDC: THandle; const hFont: THandle; out pdwCodePages: LongWord): HResult; stdcall;
    function MapFont(const hDC: THandle; const dwCodePages: LongWord; hSrcFont: THandle; out phDestFont: THandle): HResult; stdcall;
    function ReleaseFont(const hFont: THandle): HResult; stdcall;
    function ResetFontMapping: HResult; stdcall;
  end;

var
  FontLink: IMLangFontLink;

{ TdxTrueTypeFontInfo }

constructor TdxTrueTypeFontInfo.Create(const AFontName: string; const AFontType: Integer);
begin
  inherited Create;
  FFontName := AFontName;
  FFontType := AFontType;
end;

function TdxTrueTypeFontInfo.GetSupportedFontStyles: TdxSupportedFontStylesInfo;
begin
  Result := TdxGdiFontHelper.GetFontStyles(FFontName);
  FSupportedStylesCalculated := True;
  Result.NativeStyles[TdxSupportedFontStyle.Regular] := True;
  if Result.SupportedStyles = [TdxSupportedFontStyle.Italic] then
  begin
    Result.SupportedStyles := [TdxSupportedFontStyle.Regular, TdxSupportedFontStyle.BoldItalic];
    Exit;
  end;
  if Result.SupportedStyles = [TdxSupportedFontStyle.Bold] then
  begin
    Result.SupportedStyles := [TdxSupportedFontStyle.Regular, TdxSupportedFontStyle.BoldItalic];
    Exit;
  end;
  if (TdxSupportedFontStyle.Regular in Result.SupportedStyles) or (Result.SupportedStyles = []) then
    Result.SupportedStyles := [TdxSupportedFontStyle.Regular, TdxSupportedFontStyle.Bold, TdxSupportedFontStyle.Italic, TdxSupportedFontStyle.BoldItalic]
  else
    Include(Result.SupportedStyles, TdxSupportedFontStyle.BoldItalic);
end;

function TdxTrueTypeFontInfo.GetStylesInfo: TdxSupportedFontStylesInfo;
begin
  if not FSupportedStylesCalculated then
    FSupportedStyles := GetSupportedFontStyles;
  Result := FSupportedStyles;
end;

{ TdxGdiFontCache }

class procedure TdxGdiFontCache.Initialize;
begin
  FNameToCharacterSetMap := TObjectDictionary<string, TdxFontCharacterSet>.Create([doOwnsValues], 512);
  FSystemTrueTypeFonts := TdxSystemTrueTypeFontDictionary.Create([doOwnsValues]);
  FUnicodeRangeInfo := TdxUnicodeRangeInfo.Create;
  PopulateNameToCharacterMap;
end;

class procedure TdxGdiFontCache.Finalize;
begin
  FLoader.Terminate;
  FLoader.WaitFor;
  FLoader.Free;
  FUnicodeRangeInfo.Free;
  FSystemTrueTypeFonts.Free;
  FNameToCharacterSetMap.Free;
end;

function TdxGdiFontCache.CreateOverrideFontSubstitutes: TdxSystemTrueTypeFontDictionary;
begin
  Result := TdxSystemTrueTypeFontDictionary.Create([doOwnsValues]);
end;

class function TdxGdiFontCache.CreateSystemTrueTypeFonts: TStrings;
var
  AValue: TdxTrueTypeFontInfo;
  AStrings: TStringList;
begin
  AStrings := TStringList.Create;
  for AValue in FSystemTrueTypeFonts.Values do
    AStrings.AddObject(AValue.FontName, AValue);
  AStrings.Sort;
  Result := AStrings;
end;

function TdxGdiFontCache.CreateFontInfoMeasurer(AUnitConverter: TdxDocumentLayoutUnitConverter): TdxFontInfoMeasurer;
begin
  Result := TdxGdiFontInfoMeasurer.Create(AUnitConverter);
end;

function TdxGdiFontCache.CreateFontInfoCore(const AFontName: string; ADoubleFontSize: Integer;
  const AFontStyle: TFontStyles): TdxFontInfo;
begin
  Result := TdxGdiFontInfo.Create(Measurer, AFontName, ADoubleFontSize, AFontStyle);
end;

function TdxGdiFontCache.GetFontCharacterRanges(AFontInfo: TdxFontInfo): TdxFontCharacterRangeArray;
var
  AGdiFontInfo: TdxGdiFontInfo absolute AFontInfo;
begin
  Result := AGdiFontInfo.GetFontUnicodeRanges(Measurer.MeasureGraphics.Handle, AGdiFontInfo.GdiFontHandle);
end;

function TdxGdiFontCache.ShouldUseDefaultFontToDrawInvisibleCharacter(AFontInfo: TdxFontInfo; ACharacter: Char): Boolean;
var
  AGdiFontInfo: TdxGdiFontInfo absolute AFontInfo;
begin
  Result := AGdiFontInfo.CanDrawCharacter(UnicodeRangeInfo, Measurer.MeasureGraphics, ACharacter);
end;

class procedure TdxGdiFontCache.PopulateNameToCharacterMap;
begin
  FLoader := TTrueTypeFontLoader.Create(False);
end;

function TdxGdiFontCache.GetFontCharacterSet(const AFontName: string): TdxFontCharacterSet;
begin
  System.TMonitor.Enter(NameToCharacterSetMap);
  try
    if not NameToCharacterSetMap.TryGetValue(AFontName, Result) then
    begin
      Result := CreateFontCharacterSet(AFontName);
      if Result <> nil then
        NameToCharacterSetMap.Add(AFontName, Result);
    end;
  finally
    System.TMonitor.Exit(NameToCharacterSetMap);
  end;
end;

function TdxGdiFontCache.FindSubstituteFont(const ASourceFontName: string; ACharacter: Char; var AFontCharacterSet: TdxFontCharacterSet): string;
var
  AMinDistance, ADistance: Integer;
  ASourceCharacterSet, ATestFontCharacterSet: TdxFontCharacterSet;
  AFontCharacterSetPair: TPair<string, TdxFontCharacterSet>;
  AUpperFontName: string;
  ACharCodePages, ACodePage: DWORD;
begin
  Result := ASourceFontName;
  System.TMonitor.Enter(NameToCharacterSetMap);
  try
  {$IFDEF DELPHIXE4}
    AUpperFontName := ASourceFontName.ToUpper;
  {$ELSE}
    AUpperFontName := ToUpper(ASourceFontName);
  {$ENDIF}
    if not FSystemTrueTypeFonts.ContainsKey(AUpperFontName) then
    begin
      if FontLink.GetCharCodePages(ACharacter, ACharCodePages) = S_OK then
        if FontLink.CodePagesToCodePage(ACharCodePages, CP_ACP, ACodePage) = S_OK then
        begin
          AUpperFontName := '';
          case ACodePage of
            1250..1259: AUpperFontName := 'TIMES NEW ROMAN';
            932: AUpperFontName := 'MS MINCHO';
            949: AUpperFontName := 'BATANG';
            936: AUpperFontName := 'MS SONG';
            950: AUpperFontName := 'NEW MINGLIU';
            874: AUpperFontName := 'TAHOMA';
          end;
          if (AUpperFontName <> '') and FSystemTrueTypeFonts.ContainsKey(AUpperFontName) then
          begin
            AFontCharacterSet := NameToCharacterSetMap.Items[AUpperFontName];
            if AFontCharacterSet.ContainsChar(ACharacter) then
              Exit(AUpperFontName);
          end;
        end;
    end;
    AMinDistance := MaxInt;
    ASourceCharacterSet := NameToCharacterSetMap[ASourceFontName];
    for AFontCharacterSetPair in NameToCharacterSetMap do
    begin
      ATestFontCharacterSet := AFontCharacterSetPair.Value;
      if ATestFontCharacterSet.ContainsChar(ACharacter) then
      begin
        ADistance := TdxFontCharacterSet.CalculatePanoseDistance(ASourceCharacterSet, ATestFontCharacterSet);
        if ADistance < AMinDistance then
        begin
          AMinDistance := ADistance;
          Result := AFontCharacterSetPair.Key;
        end;
      end;
    end;
  finally
    System.TMonitor.Exit(NameToCharacterSetMap);
  end;
end;

function TdxGdiFontCache.GetMeasurer: TdxGdiFontInfoMeasurer;
begin
  Result := TdxGdiFontInfoMeasurer(inherited Measurer);
end;

function TdxGdiFontCache.CreateFontCharacterSet(const AFontName: string): TdxFontCharacterSet;
var
  AFontInfo: TdxGdiFontInfo;
  ACharacterRanges: TdxFontCharacterRangeArray;
begin
  AFontInfo := TdxGdiFontInfo.Create(Measurer, AFontName, 20, []);
  try
    ACharacterRanges := GetFontCharacterRanges(AFontInfo);
    Result := TdxFontCharacterSet.Create(ACharacterRanges, AFontInfo.Panose);
  finally
    AFontInfo.Free;
    ACharacterRanges := nil;
  end;
end;

{ TdxGdiFontCache.TTrueTypeFontLoader }

class function TdxGdiFontCache.TTrueTypeFontLoader.EnumFontInfoProc(
  var ALogFont: TLogFontW; ATextMetric: PTextMetricW; AFontType: Integer;
  AData: Pointer): Integer;
var
  ATemp: string;
  AFontDictionary: TdxSystemTrueTypeFontDictionary;
  ACallbackData: PCallbackData absolute AData;
begin
  if AFontType = TRUETYPE_FONTTYPE then
  begin
    AFontDictionary := TdxGdiFontCache.SystemTrueTypeFonts;
  {$IFDEF DELPHIXE4}
    ATemp  := string(PChar(@ALogFont.lfFaceName)).ToUpper;
  {$ELSE}
    ATemp  := ToUpper(PChar(@ALogFont.lfFaceName));
  {$ENDIF}
    if (ATemp <> '') and (ATemp[1] <> '@') then
    begin
      if not AFontDictionary.ContainsKey(ATemp) then
      begin
        if ALogFont.lfCharSet = SYMBOL_CHARSET then
          AFontType := AFontType or TdxTrueTypeFontInfo.SymbolFontType;
        if ALogFont.lfPitchAndFamily = FIXED_PITCH then
          AFontType := AFontType or TdxTrueTypeFontInfo.FixedPitchFontType;
        AFontDictionary.Add(ATemp, TdxTrueTypeFontInfo.Create(PChar(@ALogFont.lfFaceName), AFontType));
      end;
    end;
  end;
  Result := Ord(ACallbackData.Thread.CanContinue);
end;

procedure TdxGdiFontCache.TTrueTypeFontLoader.Execute;
var
  ALogFont: TLogFontW;
  ADC: HDC;
  I: string;
  AFontCharacterSet: TdxFontCharacterSet;
  AFontDictionary: TdxSystemTrueTypeFontDictionary absolute TdxGdiFontCache.SystemTrueTypeFonts;
  ACallbackData: TCallbackData;
begin
  System.TMonitor.Enter(TdxGdiFontCache.NameToCharacterSetMap);
  ADC := CreateCompatibleDC(0);
  try
    ACallbackData.Thread := Self;
    FillChar(ALogFont, SizeOf(ALogFont), 0);
    ALogFont.lfCharset := DEFAULT_CHARSET;
    EnumFontFamiliesExW(ADC, ALogFont, @EnumFontInfoProc, TdxNativeInt(@ACallbackData), 0);
    if CanContinue then
    begin
      for I in AFontDictionary.Keys do
      begin
        AFontCharacterSet := CreateFontCharacterSet(ADC, I);
        if AFontCharacterSet <> nil then
          TdxGdiFontCache.NameToCharacterSetMap.Add(I, AFontCharacterSet);
        if not CanContinue then
          Break;
      end;
    end;
  finally
    DeleteDC(ADC);
    System.TMonitor.Exit(TdxGdiFontCache.NameToCharacterSetMap);
  end;
end;

function TdxGdiFontCache.TTrueTypeFontLoader.CanContinue: Boolean;
begin
  Result := not Terminated and ((Application = nil) or not Application.Terminated);
end;

function TdxGdiFontCache.TTrueTypeFontLoader.CreateFontCharacterSet(ADC: HDC; const AFontName: string): TdxFontCharacterSet;
var
  AFont, AOldFont: HFONT;
  ACharacterRanges: TdxFontCharacterRangeArray;
  AOutlineTextMetrics: TOutlineTextmetric;
  ADummy: string;
begin
  AFont := TdxGdiFontHelper.CreateFont(ADC, AFontName, 22, [], 1, ADummy);
  if AFont = 0 then
    Exit(nil);
  AOldFont := SelectObject(ADC, AFont);
  try
    ACharacterRanges := TdxGdiFontInfo.GetFontUnicodeRanges(ADC, 0);
    AOutlineTextMetrics.otmSize := SizeOf(AOutlineTextMetrics);
    if Windows.GetOutlineTextMetricsW(ADC, SizeOf(AOutlineTextMetrics), @AOutlineTextMetrics) <> 0 then
      Result := TdxFontCharacterSet.Create(ACharacterRanges, AOutlineTextMetrics.otmPanoseNumber)
    else
      Result := nil;
  finally
    SelectObject(ADC, AOldFont);
    DeleteObject(AFont);
    ACharacterRanges := nil;
  end;
end;

{ TdxGdiFontCacheManager }

procedure Initialize;
begin
  TdxGdiFontCache.Initialize;
  CoInitialize(nil);
  FontLink := CreateComObject(CLASS_CMultiLanguage) as IMLangFontLink;
end;

procedure Finalize;
begin
  if FontLink <> nil then
    FontLink.ResetFontMapping;
  FontLink := nil;
  CoUninitialize;
  TdxGdiFontCache.Finalize;
end;

initialization
  dxUnitsLoader.AddUnit(@Initialize, @Finalize);

finalization
  dxUnitsLoader.RemoveUnit(@Finalize);

end.
