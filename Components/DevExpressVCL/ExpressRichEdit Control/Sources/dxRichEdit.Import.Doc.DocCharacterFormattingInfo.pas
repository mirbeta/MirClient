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
unit dxRichEdit.Import.Doc.DocCharacterFormattingInfo;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses, dxCoreGraphics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Platform.Font,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.CharacterFormatting;

type
  TdxDocBoolWrapper = (
    False   = $00,
    True    = $01,
    Leave   = $80,
    Inverse = $81
  );

  { TdxDocCharacterFormattingInfo }

  TdxDocCharacterFormattingInfo = class(TdxCloneable)
  public const
    DefaultFontName: string = 'Times New Roman';
  strict private
    FFontName: string;
    FDoubleFontSize: Integer;
    FFontBold: TdxDocBoolWrapper;
    FFontItalic: TdxDocBoolWrapper;
    FStrike: TdxDocBoolWrapper;
    FDoubleStrike: TdxDocBoolWrapper;
    FFontUnderlineType: TdxUnderlineType;
    FAllCaps: TdxDocBoolWrapper;
    FUnderlineWordsOnly: Boolean;
    FStrikeoutWordsOnly: Boolean;
    FScript: TdxCharacterFormattingScript;
    FForeColor: TdxAlphaColor;
    FBackColor: TdxAlphaColor;
    FUnderlineColor: TdxAlphaColor;
    FStrikeoutColor: TdxAlphaColor;
    FHidden: TdxDocBoolWrapper;
    FStyleIndex: Integer;
    procedure SetDoubleFontSize(const AValue: Integer);
  public
    constructor Create; override;
    class function CreateDefault: TdxDocCharacterFormattingInfo; static;
    function Clone: TdxDocCharacterFormattingInfo; reintroduce; inline;
    procedure CopyFrom(Source: TdxCloneable); override;
    function Equals(AObj: TObject): Boolean; override;
    function ContainsInvertedProperties: Boolean;

    property FontName: string read FFontName write FFontName;
    property DoubleFontSize: Integer read FDoubleFontSize write SetDoubleFontSize;
    property FontBold: TdxDocBoolWrapper read FFontBold write FFontBold;
    property FontItalic: TdxDocBoolWrapper read FFontItalic write FFontItalic;
    property Strike: TdxDocBoolWrapper read FStrike write FStrike;
    property DoubleStrike: TdxDocBoolWrapper read FDoubleStrike write FDoubleStrike;
    property FontUnderlineType: TdxUnderlineType read FFontUnderlineType write FFontUnderlineType;
    property AllCaps: TdxDocBoolWrapper read FAllCaps write FAllCaps;
    property UnderlineWordsOnly: Boolean read FUnderlineWordsOnly write FUnderlineWordsOnly;
    property StrikeoutWordsOnly: Boolean read FStrikeoutWordsOnly write FStrikeoutWordsOnly;
    property ForeColor: TdxAlphaColor read FForeColor write FForeColor;
    property BackColor: TdxAlphaColor read FBackColor write FBackColor;
    property UnderlineColor: TdxAlphaColor read FUnderlineColor write FUnderlineColor;
    property StrikeoutColor: TdxAlphaColor read FStrikeoutColor write FStrikeoutColor;
    property Script: TdxCharacterFormattingScript read FScript write FScript;
    property Hidden: TdxDocBoolWrapper read FHidden write FHidden;
    property StyleIndex: Integer read FStyleIndex write FStyleIndex;
  end;

implementation

uses
  dxRichEdit.Utils.Exceptions;

{ TdxDocCharacterFormattingInfo }

constructor TdxDocCharacterFormattingInfo.Create;
begin
  inherited Create;
  FFontName := '';
  FForeColor := TdxAlphaColors.Empty;
  FBackColor := TdxAlphaColors.Transparent;
  FUnderlineColor := TdxAlphaColors.Empty;
  FStrikeoutColor := TdxAlphaColors.Empty;
  FStyleIndex := $0a;
end;

class function TdxDocCharacterFormattingInfo.CreateDefault: TdxDocCharacterFormattingInfo;
begin
  Result := TdxDocCharacterFormattingInfo.Create;
  Result.FontName := DefaultFontName;
  Result.DoubleFontSize := $000c * 2;
  Result.Script := TdxCharacterFormattingScript.Normal;
end;

procedure TdxDocCharacterFormattingInfo.SetDoubleFontSize(const AValue: Integer);
begin
  if AValue <= 0 then
    TdxRichEditExceptions.ThrowArgumentException('DoubleFontSize', AValue);
  FDoubleFontSize := AValue;
end;

function TdxDocCharacterFormattingInfo.Equals(AObj: TObject): Boolean;
var
  AInfo: TdxDocCharacterFormattingInfo absolute AObj;
begin
  Result :=
    (DoubleFontSize = AInfo.DoubleFontSize) and
    (FontBold = AInfo.FontBold) and
    (FontItalic = AInfo.FontItalic) and
    (Strike = AInfo.Strike) and
    (DoubleStrike = AInfo.DoubleStrike) and
    (FontUnderlineType = AInfo.FontUnderlineType) and
    (AllCaps = AInfo.AllCaps) and
    (ForeColor = AInfo.ForeColor) and
    (BackColor = AInfo.BackColor) and
    (UnderlineColor = AInfo.UnderlineColor) and
    (StrikeoutColor = AInfo.StrikeoutColor) and
    (StrikeoutWordsOnly = AInfo.StrikeoutWordsOnly) and
    (UnderlineWordsOnly = AInfo.UnderlineWordsOnly) and
    (Script = AInfo.Script) and
    (Hidden = AInfo.Hidden) and
    (StyleIndex = AInfo.StyleIndex) and
    SameText(FontName, AInfo.FontName);
end;

function TdxDocCharacterFormattingInfo.Clone: TdxDocCharacterFormattingInfo;
begin
  Result := TdxDocCharacterFormattingInfo(inherited Clone);
end;

function TdxDocCharacterFormattingInfo.ContainsInvertedProperties: Boolean;
begin
  Result :=
    (FAllCaps = TdxDocBoolWrapper.Inverse) or
    (FFontBold = TdxDocBoolWrapper.Inverse) or
    (FFontItalic = TdxDocBoolWrapper.Inverse) or
    (FHidden = TdxDocBoolWrapper.Inverse) or
    (FStrike = TdxDocBoolWrapper.Inverse) or
    (FDoubleStrike = TdxDocBoolWrapper.Inverse);
end;

procedure TdxDocCharacterFormattingInfo.CopyFrom(Source: TdxCloneable);
var
  AInfo: TdxDocCharacterFormattingInfo absolute Source;
begin
  FontName := AInfo.FontName;
  DoubleFontSize := AInfo.DoubleFontSize;
  FontBold := AInfo.FontBold;
  FontItalic := AInfo.FontItalic;
  Strike := AInfo.Strike;
  DoubleStrike := AInfo.DoubleStrike;
  FontUnderlineType := AInfo.FontUnderlineType;
  AllCaps := AInfo.AllCaps;
  ForeColor := AInfo.ForeColor;
  BackColor := AInfo.BackColor;
  UnderlineColor := AInfo.UnderlineColor;
  StrikeoutColor := AInfo.StrikeoutColor;
  UnderlineWordsOnly := AInfo.UnderlineWordsOnly;
  StrikeoutWordsOnly := AInfo.StrikeoutWordsOnly;
  Script := AInfo.Script;
  Hidden := AInfo.Hidden;
  StyleIndex := AInfo.StyleIndex;
end;

end.
