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

unit dxRichEdit.Import.Html.TextTags;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  dxCoreGraphics,
  dxRichEdit.Import,
  dxRichEdit.Import.Html.TagBase,
  dxRichEdit.Import.Html.HtmlTags,
  dxRichEdit.Import.Html.DocumentTags;

type
  { TdxBoldTag }

  TdxBoldTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

  { TdxItalicTag }

  TdxItalicTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

  { TdxUnderlineTag }

  TdxUnderlineTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

  { TdxStrongTag }

  TdxStrongTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

  { TdxBigTag }

  TdxBigTag = class(TdxHtmlTag)
  protected
    procedure ApplyTagProperties; override;
  end;

  { TdxSmallTag }

  TdxSmallTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

  { TdxFontTag }

  TdxFontTag = class(TdxTagBase)
  strict private
    class var
      FAttributeTable: TdxAttributeKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FDoubleFontSize: Integer;
    FUseFontSize: Boolean;
    FFontName: string;
    FUseFontName: Boolean;
    FFontColor: TdxAlphaColor;
    FUseFontColor: Boolean;
    procedure SetDoubleFontSize(const AValue: Integer);
    procedure SetFontName(const AValue: string);
    procedure SetFontColor(const AValue: TdxAlphaColor);
  protected
    function GetAttributeTable: TdxAttributeKeywordTranslatorTable; override;
    class function AddAttributes: TdxAttributeKeywordTranslatorTable; static;
    class procedure FontSizeKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure FontColorKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure FontFaceKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    procedure ApplyTagProperties; override;

    property DoubleFontSize: Integer read FDoubleFontSize write SetDoubleFontSize;
    property FontName: string read FFontName write SetFontName;
    property FontColor: TdxAlphaColor read FFontColor write SetFontColor;
  public
    constructor Create(AImporter: TdxCustomHtmlImporter);
  end;

  { TdxEmphasizedTag }

  TdxEmphasizedTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

  { TdxSubScriptTag }

  TdxSubScriptTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

  { TdxSuperScriptTag }

  TdxSuperScriptTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

  { TdxStrikeoutTag }

  TdxStrikeoutTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

  { TdxCenterTag }

  TdxCenterTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  public
    procedure FunctionalTagProcess; override;
  end;

  { TdxCodeTag }

  TdxCodeTag = class(TdxHtmlTag)
  protected
    procedure ApplyTagProperties; override;
  end;

  { TdxBlockquoteTag }

  TdxBlockquoteTag = class(TdxTagBase)
  strict private
    FIndent: Integer;
  protected
    procedure ApplyTagProperties; override;
  public
    constructor Create(AImporter: TdxCustomHtmlImporter);
    procedure FunctionalTagProcess; override;
  end;

  { TdxXmpTag }

  TdxXmpTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

  { TdxWbrTag }

  TdxWbrTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

  { TdxVarTag }

  TdxVarTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

  { TdxTtTag }

  TdxTtTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

  { TdxSampTag }

  TdxSampTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

  { TdxQTag }

  TdxQTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
    procedure OpenTagProcessCore; override;
  public
    procedure BeforeDeleteTagFromStack(AIndexOfDeletedTag: Integer); override;
  end;

  { TdxNobrTag }

  TdxNobrTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

  { TdxKbdTag }

  TdxKbdTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

  { TdxInsTag }

  TdxInsTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

  { TdxDfnTag }

  TdxDfnTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

  { TdxDelTag }

  TdxDelTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

  { TdxCiteTag }

  TdxCiteTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

  { TdxBdoTag }

  TdxBdoTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

  { TdxBaseFontTag }

  TdxBaseFontTag = class(TdxFontTag);

  { TdxAddressTag }

  TdxAddressTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

  { TdxAcronymTag }

  TdxAcronymTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

  { TdxAbberTag }

  TdxAbberTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;


implementation

uses
  SysUtils,
  dxRichEdit.NativeApi,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.Import.Html,
  dxRichEdit.Platform.Font,
  dxStringHelper,
  dxRichEdit.Utils.NumberParser,
  dxRichEdit.DocumentModel.PieceTable;

{ TdxBoldTag }

procedure TdxBoldTag.ApplyTagProperties;
begin
   TdxHtmlImporter(Importer).Position.CharacterFormatting.FontBold := True;
end;

{ TdxItalicTag }

procedure TdxItalicTag.ApplyTagProperties;
begin
  TdxHtmlImporter(Importer).Position.CharacterFormatting.FontItalic := True;
end;

{ TdxUnderlineTag }

procedure TdxUnderlineTag.ApplyTagProperties;
begin
  TdxHtmlImporter(Importer).Position.CharacterFormatting.FontUnderlineType := TdxUnderlineType.Single;
end;

{ TdxStrongTag }

procedure TdxStrongTag.ApplyTagProperties;
begin
  TdxHtmlImporter(Importer).Position.CharacterFormatting.FontBold := True;
end;

{ TdxBigTag }

procedure TdxBigTag.ApplyTagProperties;
begin
  TdxHtmlImporter(Importer).Position.CharacterFormatting.DoubleFontSize := TdxHtmlImporter(Importer).HtmlFontSize.GetLargerDoubleFontSize;
end;

{ TdxSmallTag }

procedure TdxSmallTag.ApplyTagProperties;
begin
  TdxHtmlImporter(Importer).Position.CharacterFormatting.DoubleFontSize := TdxHtmlImporter(Importer).HtmlFontSize.GetSmallerDoubleFontSize;
end;

{ TdxFontTag }

constructor TdxFontTag.Create(AImporter: TdxCustomHtmlImporter);
var
  AFormatting: TdxCharacterFormattingBase;
begin
  inherited Create(AImporter);
  AFormatting := TdxHtmlImporter(Importer).Position.CharacterFormatting;
  if AFormatting.Options.UseDoubleFontSize then
    DoubleFontSize := AFormatting.DoubleFontSize;
  if AFormatting.Options.UseFontName then
    FontName := AFormatting.FontName;

  if AFormatting.Options.UseForeColor then
    FontColor := AFormatting.ForeColor;
end;

class constructor TdxFontTag.Initialize;
begin
  FAttributeTable := AddAttributes;
end;

class destructor TdxFontTag.Finalize;
begin
  FAttributeTable.Free;
end;

function TdxFontTag.GetAttributeTable: TdxAttributeKeywordTranslatorTable;
begin
  Result := FAttributeTable;
end;

class function TdxFontTag.AddAttributes: TdxAttributeKeywordTranslatorTable;
begin
  Result := CreateAttributeTable;
  Result.Add(ConvertKeyToUpper('size'), FontSizeKeyword);
  Result.Add(ConvertKeyToUpper('color'), FontColorKeyword);
  Result.Add(ConvertKeyToUpper('face'), FontFaceKeyword);
end;

class procedure TdxFontTag.FontSizeKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  AFontTag: TdxFontTag;
  ACount, I: Integer;
begin
  AFontTag := TdxFontTag(ATag);
  try
    if TdxStringHelper.StartsWithChar(AValue, '+', False) then
    begin
      ACount := StrToInt(TdxStringHelper.Substring(AValue, 1));
      for I := 0 to ACount - 1 do
        AFontTag.DoubleFontSize := TdxHtmlImporter(AImporter).HtmlFontSize.GetLargerDoubleFontSize;
    end
    else
      if TdxStringHelper.StartsWithChar(AValue, '-', False) then
      begin
        ACount := StrToInt(TdxStringHelper.Substring(AValue, 1));
        for I := 0 to ACount - 1 do
          AFontTag.DoubleFontSize := TdxHtmlImporter(AImporter).HtmlFontSize.GetSmallerDoubleFontSize;
      end
      else
        AFontTag.DoubleFontSize := TdxHtmlImporter(AImporter).HtmlFontSize.GetDoubleFontSize(StrToInt(AValue));
  except

  end;
end;

class procedure TdxFontTag.FontColorKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  AFontTag: TdxFontTag;
begin
  AFontTag := TdxFontTag(ATag);
  AFontTag.FontColor := TdxMarkupLanguageColorParser.ParseColor(AValue);
end;

class procedure TdxFontTag.FontFaceKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  AFontTag: TdxFontTag;
  ANames: TArray<string>;
  I: Integer;
  AName: string;
begin
  AFontTag := TdxFontTag(ATag);
  if AValue = '' then
  begin
    AFontTag.FontName := AValue;
    Exit;
  end;

  ANames := TdxStringHelper.Split(AValue, [',']);
  for I := 0 to Length(ANames) - 1 do
  begin
    AName := Trim(ANames[I]);
    if AName <> '' then
    begin
      AFontTag.FontName := AName;
      Break;
    end;
  end;
end;

procedure TdxFontTag.SetDoubleFontSize(const AValue: Integer);
begin
  FDoubleFontSize := AValue;
  FUseFontSize := True;
end;

procedure TdxFontTag.SetFontName(const AValue: string);
begin
  FFontName := AValue;
  FUseFontName := True;
end;

procedure TdxFontTag.SetFontColor(const AValue: TdxAlphaColor);
begin
  FFontColor := AValue;
  FUseFontColor := True;
end;

procedure TdxFontTag.ApplyTagProperties;
var
  AFormatting: TdxCharacterFormattingBase;
begin
  AFormatting := TdxHtmlImporter(Importer).Position.CharacterFormatting;
  if FUseFontSize then
    AFormatting.DoubleFontSize := DoubleFontSize;
  if FUseFontName then
    AFormatting.FontName := FontName;
  if FUseFontColor then
    AFormatting.ForeColor := FontColor;
end;

{ TdxEmphasizedTag }

procedure TdxEmphasizedTag.ApplyTagProperties;
begin
  TdxHtmlImporter(Importer).Position.CharacterFormatting.FontItalic := True;
end;

{ TdxSubScriptTag }

procedure TdxSubScriptTag.ApplyTagProperties;
begin
  TdxHtmlImporter(Importer).Position.CharacterFormatting.Script := TdxCharacterFormattingScript.Subscript;
end;

{ TdxSuperScriptTag }

procedure TdxSuperScriptTag.ApplyTagProperties;
begin
  TdxHtmlImporter(Importer).Position.CharacterFormatting.Script := TdxCharacterFormattingScript.Superscript;
end;

{ TdxStrikeoutTag }

procedure TdxStrikeoutTag.ApplyTagProperties;
begin
  TdxHtmlImporter(Importer).Position.CharacterFormatting.FontStrikeoutType := TdxStrikeoutType.Single;
end;

{ TdxCenterTag }

procedure TdxCenterTag.ApplyTagProperties;
begin
  TdxHtmlImporter(Importer).Position.DefaultAlignment.AlignmentValue := TdxParagraphAlignment.Center;
  TdxHtmlImporter(Importer).Position.ParagraphFormatting.Alignment := TdxParagraphAlignment.Center;
end;

procedure TdxCenterTag.FunctionalTagProcess;
begin
  ParagraphFunctionalProcess;
end;

{ TdxCodeTag }

procedure TdxCodeTag.ApplyTagProperties;
begin
  TdxHtmlImporter(Importer).Position.CharacterFormatting.DoubleFontSize := 20;
  TdxHtmlImporter(Importer).Position.CharacterFormatting.FontName := 'Courier New';
end;

{ TdxBlockquoteTag }

constructor TdxBlockquoteTag.Create(AImporter: TdxCustomHtmlImporter);
begin
  inherited Create(AImporter);
  FIndent := AImporter.UnitConverter.PixelsToModelUnits(40, 96);
end;

procedure TdxBlockquoteTag.ApplyTagProperties;
begin
  TdxHtmlImporter(Importer).Position.AdditionalIndent := TdxHtmlImporter(Importer).Position.AdditionalIndent + FIndent;
end;

procedure TdxBlockquoteTag.FunctionalTagProcess;
begin
  ParagraphFunctionalProcess;
end;

{ TdxXmpTag }

procedure TdxXmpTag.ApplyTagProperties;
begin
end;

{ TdxWbrTag }

procedure TdxWbrTag.ApplyTagProperties;
begin
end;

{ TdxVarTag }

procedure TdxVarTag.ApplyTagProperties;
begin
end;

{ TdxTtTag }

procedure TdxTtTag.ApplyTagProperties;
begin
end;

{ TdxSampTag }

procedure TdxSampTag.ApplyTagProperties;
begin
end;

{ TdxQTag }

procedure TdxQTag.ApplyTagProperties;
begin
end;

procedure TdxQTag.OpenTagProcessCore;
begin
  Importer.PieceTable.InsertText(TdxHtmlImporter(Importer).Position, '"', False);
end;

procedure TdxQTag.BeforeDeleteTagFromStack(AIndexOfDeletedTag: Integer);
begin
  Importer.PieceTable.InsertText(TdxHtmlImporter(Importer).Position, '"', False);
  inherited BeforeDeleteTagFromStack(AIndexOfDeletedTag);
end;

{ TdxNobrTag }

procedure TdxNobrTag.ApplyTagProperties;
begin
end;

{ TdxKbdTag }

procedure TdxKbdTag.ApplyTagProperties;
begin
end;

{ TdxInsTag }

procedure TdxInsTag.ApplyTagProperties;
begin
end;

{ TdxDfnTag }

procedure TdxDfnTag.ApplyTagProperties;
begin
end;

{ TdxDelTag }

procedure TdxDelTag.ApplyTagProperties;
begin
end;

{ TdxCiteTag }

procedure TdxCiteTag.ApplyTagProperties;
begin
end;

{ TdxBdoTag }

procedure TdxBdoTag.ApplyTagProperties;
begin
end;

{ TdxAddressTag }

procedure TdxAddressTag.ApplyTagProperties;
begin
end;

{ TdxAcronymTag }

procedure TdxAcronymTag.ApplyTagProperties;
begin
end;

{ TdxAbberTag }

procedure TdxAbberTag.ApplyTagProperties;
begin
end;

end.
