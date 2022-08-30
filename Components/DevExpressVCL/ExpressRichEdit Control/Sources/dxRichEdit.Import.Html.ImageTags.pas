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

unit dxRichEdit.Import.Html.ImageTags;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Types, Generics.Defaults, Generics.Collections,
  dxRichEdit.Import,
  dxRichEdit.Import.CSSParser,
  dxRichEdit.Import.Html.TagBase,
  dxGenerics,
  dxRichEdit.Utils.DXUnit,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.Utils.OfficeImage;

type

  { TdxAreaTag }

  TdxAreaTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

  { TdxMapTag }

  TdxMapTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

  { TdxImageTag }

  TdxImageTag = class(TdxTagBase)
  strict private
    class var
      FAttributeTable: TdxAttributeKeywordTranslatorTable;
      FAlignments: TdxNamedOrdinalDictionary<TdxHtmlImageAlignment>;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FUri: string;
    FAlignment: TdxHtmlImageAlignment;
    FWidthInfo: TdxValueInfo;
    FHeightInfo: TdxValueInfo;
    class function CreateAllignmentsTable: TdxNamedOrdinalDictionary<TdxHtmlImageAlignment>; static;
    class function AddAttributes: TdxAttributeKeywordTranslatorTable; static;
    class procedure ImageAlignmentKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure AlternativeTextKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure BorderWidthKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure ImageHeightKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure HorizontalIndentKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure IsImageOnServerKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure PreImageURLKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
  protected
    class procedure ImageURLKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure VerticalIndentKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure ImageWidthKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class function CalculateValueInfo(AImporter: TdxCustomHtmlImporter; const AValue: string): TdxValueInfo; static;
    function GetAttributeTable: TdxAttributeKeywordTranslatorTable; override;
    function GetApplyStylesToInnerHtml: Boolean; override;
    function GetUri: string; virtual;
    procedure SetUri(const AValue: string); virtual;
    procedure ApplyTagProperties; override;
    function ApplyCssProperties: TdxParagraphFormattingOptions; override;
    function CalculateWidthUnitInfo(const AInfo: TdxValueInfo): TdxWidthUnitInfo;
    procedure OpenTagProcessCore; override;
    function GetActualAlignment: TdxHtmlImageAlignment;
    procedure AppendPictureFloatingObjectContent(AImage: TdxOfficeImageReference; AAlignment: TdxHtmlImageAlignment;
      const ADesiredSizeInModelUnits: TSize);

    property Alignment: TdxHtmlImageAlignment read FAlignment write FAlignment;
    property WidthInfo: TdxValueInfo read FWidthInfo write FWidthInfo;
    property HeightInfo: TdxValueInfo read FHeightInfo write FHeightInfo;
    property Uri: string read GetUri write SetUri;
  public
    constructor Create(AImporter: TdxCustomHtmlImporter);
    procedure EmptyTagProcess; override;
  end;


implementation

uses
  Math, dxTypeHelpers,
  dxRichEdit.DocumentModel.FloatingObjectFormatting,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.Import.Html.CSSParser,
  dxRichEdit.Import.Html,
  dxRichEdit.Utils.Graphics,
  dxRichEdit.Utils.NumberParser,
  dxStringHelper,
  dxRichEdit.Utils.Types,
  dxUriRecord,
  dxRichEdit.DocumentModel.Core;

type
  TdxImageTagHelper = class helper for TdxImageTag
  private
    function GetImporter: TdxHtmlImporter;
  public
    property Importer: TdxHtmlImporter read GetImporter;
  end;

{ TdxImageTagHelper }

function TdxImageTagHelper.GetImporter: TdxHtmlImporter;
begin
  Result := TdxHtmlImporter(inherited Importer);
end;

{ TdxAreaTag }

procedure TdxAreaTag.ApplyTagProperties;
begin
end;

{ TdxMapTag }

procedure TdxMapTag.ApplyTagProperties;
begin
end;

{ TdxImageTag }

class constructor TdxImageTag.Initialize;
begin
  FAttributeTable := AddAttributes;
  FAlignments := CreateAllignmentsTable;
end;

class destructor TdxImageTag.Finalize;
begin
  FAttributeTable.Free;
  FAlignments.Free;
end;

constructor TdxImageTag.Create(AImporter: TdxCustomHtmlImporter);
begin
  inherited Create(AImporter);
  FAlignment := TdxHtmlImageAlignment.None;
end;

class function TdxImageTag.CreateAllignmentsTable: TdxNamedOrdinalDictionary<TdxHtmlImageAlignment>;
begin
  Result := TdxNamedOrdinalDictionary<TdxHtmlImageAlignment>.Create;
  Result.Add('left', TdxHtmlImageAlignment.Left);
  Result.Add('right', TdxHtmlImageAlignment.Right);
  Result.Add('top', TdxHtmlImageAlignment.Top);
  Result.Add('bottom', TdxHtmlImageAlignment.Bottom);
  Result.Add('middle', TdxHtmlImageAlignment.Middle);
end;

class function TdxImageTag.AddAttributes: TdxAttributeKeywordTranslatorTable;
begin
  Result := CreateAttributeTable;
  Result.Add(ConvertKeyToUpper('align'), ImageAlignmentKeyword);
  Result.Add(ConvertKeyToUpper('alt'), AlternativeTextKeyword);
  Result.Add(ConvertKeyToUpper('border'), BorderWidthKeyword);
  Result.Add(ConvertKeyToUpper('height'), ImageHeightKeyword);
  Result.Add(ConvertKeyToUpper('hspace'), HorizontalIndentKeyword);
  Result.Add(ConvertKeyToUpper('ismap'), IsImageOnServerKeyword);
  Result.Add(ConvertKeyToUpper('lowsec'), PreImageURLKeyword);
  Result.Add(ConvertKeyToUpper('src'), ImageURLKeyword);
  Result.Add(ConvertKeyToUpper('vspace'), VerticalIndentKeyword);
  Result.Add(ConvertKeyToUpper('width'), ImageWidthKeyword);
end;

class procedure TdxImageTag.ImageAlignmentKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  AAlignment: TdxHtmlImageAlignment;
  AImageTag: TdxImageTag;
begin
  if not FAlignments.TryGetValue(LowerCase(AValue), AAlignment) then
    AAlignment := TdxHtmlImageAlignment.None;
  AImageTag := TdxImageTag(ATag);

  AImageTag.Alignment := AAlignment;
end;

class procedure TdxImageTag.AlternativeTextKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
begin
end;

class procedure TdxImageTag.BorderWidthKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
begin
end;

class procedure TdxImageTag.ImageHeightKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  AImageTag: TdxImageTag;
begin
  AImageTag := TdxImageTag(ATag);

  AImageTag.HeightInfo := CalculateValueInfo(AImporter, AValue);
end;

class procedure TdxImageTag.HorizontalIndentKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
begin
end;

class procedure TdxImageTag.IsImageOnServerKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
begin
end;

class procedure TdxImageTag.PreImageURLKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
begin
end;

class procedure TdxImageTag.ImageURLKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  AImageTag: TdxImageTag;
begin
  AImageTag := TdxImageTag(ATag);
  if TdxStringHelper.StartsWith(AValue, 'data:') then
    AImageTag.Uri := AValue
  else
  begin
    try
      AImageTag.Uri := ATag.GetAbsoluteUri(TdxHtmlImporter(AImporter).AbsoluteBaseUri, TdxUri.UnescapeDataString(AValue));
    except
    end;
  end;
end;

class procedure TdxImageTag.VerticalIndentKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
begin
end;

class procedure TdxImageTag.ImageWidthKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  AImageTag: TdxImageTag;
begin
  AImageTag := TdxImageTag(ATag);

  AImageTag.WidthInfo := CalculateValueInfo(AImporter, AValue);
end;

class function TdxImageTag.CalculateValueInfo(AImporter: TdxCustomHtmlImporter; const AValue: string): TdxValueInfo;
var
  AValueInPixels: Integer;
  AParsedValue: TdxLengthValueParser;
begin
  if TdxNumber.TryParse(AValue, AValueInPixels) then
    AParsedValue := TdxLengthValueParser.Create(AValue + 'px', AImporter.DocumentModel.ScreenDpi)
  else
    AParsedValue := TdxLengthValueParser.Create(AValue, AImporter.DocumentModel.ScreenDpi);

  if AParsedValue.IsRelativeUnit or not AParsedValue.IsDigit then
    Exit(TdxValueInfo.Create(AParsedValue.Value, AParsedValue.&Unit));
  Result := TdxValueInfo.Create(AParsedValue.PointsValue, 'pt');
end;

function TdxImageTag.GetAttributeTable: TdxAttributeKeywordTranslatorTable;
begin
  Result := FAttributeTable;
end;

function TdxImageTag.GetApplyStylesToInnerHtml: Boolean;
begin
  Result := False;
end;

function TdxImageTag.GetUri: string;
begin
  Result := FUri;
end;

procedure TdxImageTag.SetUri(const AValue: string);
begin
  FUri := AValue;
end;

procedure TdxImageTag.ApplyTagProperties;
var
  AWidth, AHeight: TdxWidthUnitInfo;
begin
  AWidth := CalculateWidthUnitInfo(FWidthInfo);
  try
    Importer.Position.ImageProperties.Width := AWidth;
  finally
    AWidth.Free;
  end;
  AHeight := CalculateWidthUnitInfo(FHeightInfo);
  try
    Importer.Position.ImageProperties.Height := AHeight;
  finally
    AHeight.Free;
  end;
  Importer.Position.ImageProperties.Alignment := FAlignment;
end;

function TdxImageTag.ApplyCssProperties: TdxParagraphFormattingOptions;
begin
  Importer.Position.ImageProperties.ResetFloat;
  Result := inherited ApplyCssProperties;
end;

function TdxImageTag.CalculateWidthUnitInfo(const AInfo: TdxValueInfo): TdxWidthUnitInfo;
var
  AFontSize, ARootDoubleFontSize: Integer;
begin
  if not AInfo.IsEmpty then
  begin
    if AInfo.&Unit <> 'pt' then
    begin
      AFontSize := Importer.Position.CharacterFormatting.DoubleFontSize div 2;
      ARootDoubleFontSize := Importer.RootDoubleFontSize;
      Exit(TdxTagBaseHelper.GetRelativeWidth(Importer.UnitConverter, AInfo.&Unit, AInfo.Value, AFontSize, ARootDoubleFontSize));
    end;
    Result := TdxWidthUnitInfo.Create;
    Result.Value := Max(0, Round(Importer.UnitConverter.PointsToModelUnitsF(AInfo.Value)));
    Result.&Type := TdxWidthUnitType.ModelUnits;
  end
  else
    Result := TdxWidthUnitInfo.Create;
end;

procedure TdxImageTag.OpenTagProcessCore;
var
  AWidth, AHeight: TdxWidthUnitInfo;
  AWidthInModelUnits, AHeightInModelUnits, AWidthInPixels, AHeightInPixels: Integer;
  AImage: TdxUriOfficeImage;
  AScaleX, AScaleY: Single;
  ADesiredSizeInModelUnits: TSize;
  AAlignment: TdxHtmlImageAlignment;
begin
  AWidth := Importer.Position.ImageProperties.Width;
  if AWidth.&Type = TdxWidthUnitType.ModelUnits then
    AWidthInModelUnits := AWidth.Value
  else
    AWidthInModelUnits := 0;
  AHeight := Importer.Position.ImageProperties.Height;
  if AHeight.&Type = TdxWidthUnitType.ModelUnits then
    AHeightInModelUnits := AHeight.Value
  else
    AHeightInModelUnits := 0;

  AWidthInPixels := Importer.UnitConverter.ModelUnitsToPixels(AWidthInModelUnits, DocumentModel.Dpi);
  AHeightInPixels := Importer.UnitConverter.ModelUnitsToPixels(AHeightInModelUnits, DocumentModel.Dpi);
  if Importer.DocumentModel.DocumentCapabilities.InlinePicturesAllowed then
  begin
    AImage := Importer.CreateUriBasedRichEditImage(Uri, AWidthInPixels, AHeightInPixels);
    if AImage <> nil then
    begin
      if AImage.IsLoaded and (AImage.Image.Width > 0) and (AWidth.&Type <> TdxWidthUnitType.&Nil) then
        AScaleX := Importer.UnitConverter.ModelUnitsToTwips(AWidthInModelUnits) / AImage.Image.SizeInTwips.cx * 100
      else
        AScaleX := 100.0;

      if AImage.IsLoaded and (AImage.Image.Height > 0) and (AHeight.&Type <> TdxWidthUnitType.&Nil) then
        AScaleY := Importer.UnitConverter.ModelUnitsToTwips(AHeightInModelUnits) / AImage.Image.SizeInTwips.cy * 100
      else
        AScaleY := 100.0;

      if (AHeight.&Type = TdxWidthUnitType.&Nil) and (AScaleX <> 100.0) then
        AScaleY := AScaleX
      else
        if (AWidth.&Type = TdxWidthUnitType.&Nil) and (AScaleY <> 100.0) then
          AScaleX := AScaleY;

      ADesiredSizeInModelUnits := TSize.Create(AWidthInModelUnits, AHeightInModelUnits);

      AAlignment := GetActualAlignment;
      if (Importer.Options.IgnoreFloatProperty) or
         ((AAlignment <> TdxHtmlImageAlignment.Left) and (AAlignment <> TdxHtmlImageAlignment.Right)) then
        Importer.AppendInlineImage(AImage, AScaleX, AScaleY, ADesiredSizeInModelUnits)
      else
        AppendPictureFloatingObjectContent(AImage, AAlignment, ADesiredSizeInModelUnits);
    end;
  end
  else
    Importer.AppendText(' ');
  Importer.Position.CopyFrom(Importer.TagsStack[Importer.TagsStack.Count - 1].OldPosition);
end;

function TdxImageTag.GetActualAlignment: TdxHtmlImageAlignment;
var
  ACssFloat: TdxHtmlCssFloat;
begin
  ACssFloat := Importer.Position.ImageProperties.CssFloat;
  case ACssFloat of
    TdxHtmlCssFloat.NotSet:
      Result := FAlignment;
    TdxHtmlCssFloat.Left:
      Result := TdxHtmlImageAlignment.Left;
    TdxHtmlCssFloat.Right:
      Result := TdxHtmlImageAlignment.Right;
    else
      Result := TdxHtmlImageAlignment.None;
  end;
end;

procedure TdxImageTag.AppendPictureFloatingObjectContent(AImage: TdxOfficeImageReference; AAlignment: TdxHtmlImageAlignment;
  const ADesiredSizeInModelUnits: TSize);
var
  APieceTable: TdxPieceTable;
  ARun: TdxFloatingObjectAnchorRun;
  AContent: TdxPictureFloatingObjectContent;
  AOriginalSize: TSize;
begin
  APieceTable := Importer.PieceTable;
  ARun := APieceTable.InsertFloatingObjectAnchorCore(Importer.Position);
  Importer.SetAppendObjectProperty;
  ARun.FloatingObjectProperties.BeginUpdate;
  ARun.FloatingObjectProperties.TextWrapType := TdxFloatingObjectTextWrapType.Square;
  ARun.FloatingObjectProperties.VerticalPositionType := TdxFloatingObjectVerticalPositionType.Line;
  if AAlignment = TdxHtmlImageAlignment.Left then
  begin
    ARun.FloatingObjectProperties.TextWrapSide := TdxFloatingObjectTextWrapSide.Right;
    ARun.FloatingObjectProperties.HorizontalPositionAlignment := TdxFloatingObjectHorizontalPositionAlignment.Left;
    ARun.FloatingObjectProperties.HorizontalPositionType := TdxFloatingObjectHorizontalPositionType.Column;
  end
  else
  begin
    ARun.FloatingObjectProperties.TextWrapSide := TdxFloatingObjectTextWrapSide.Left;
    ARun.FloatingObjectProperties.HorizontalPositionAlignment := TdxFloatingObjectHorizontalPositionAlignment.Right;
    ARun.FloatingObjectProperties.HorizontalPositionType := TdxFloatingObjectHorizontalPositionType.Column;
  end;
  ARun.FloatingObjectProperties.EndUpdate;
  AContent := TdxPictureFloatingObjectContent.Create(ARun, AImage);
  AOriginalSize := AImage.CalculateImageSizeInModelUnits(DocumentModel.UnitConverter);
  AContent.SetOriginalSize(AOriginalSize);

  if (ADesiredSizeInModelUnits.Width = 0) and (ADesiredSizeInModelUnits.Height = 0) then
    ARun.FloatingObjectProperties.ActualSize := AOriginalSize
  else
    ARun.FloatingObjectProperties.ActualSize := ADesiredSizeInModelUnits;
  ARun.SetContent(AContent);
  if AImage.IsLoaded then
  begin
  end
  else
    AContent.InvalidActualSize := True;
end;

procedure TdxImageTag.EmptyTagProcess;
begin
  Importer.OpenProcess(Self);
  Importer.CloseProcess(Self);
end;

end.
