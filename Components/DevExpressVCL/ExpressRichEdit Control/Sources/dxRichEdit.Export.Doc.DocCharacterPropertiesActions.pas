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
unit dxRichEdit.Export.Doc.DocCharacterPropertiesActions;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses, dxCoreGraphics,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.DocumentModel.Styles.Core,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.Doc.Utils,
  dxRichEdit.Import.Doc.DocStyles,
  dxRichEdit.Import.Doc.DocStyleSheet,
  dxRichEdit.Import.Doc.DocPictureBulletInformation;

type
  { TdxDocCharacterPropertiesActions }

  TdxDocCharacterPropertiesActions = class
  strict private
    FWriter: TBinaryWriter;
    FFontNameIndex: Integer;
    FUseDefaultFontSize: Boolean;
    FPictureBulletInformation: TdxDocPictureBulletInformation;
    FDocumentModel: TdxDocumentModel;
    function GetActualDoubleFontSize(AUseFontSize: Boolean): Integer;
  strict protected
    FCharacterFormattingInfo: TdxCharacterFormattingInfo;
    FCharacterFormattingOptions: TdxCharacterFormattingOptions;
    procedure SetUseDefaultFontSize(ACharacterStyleIndex: Integer; AParagraphStyleIndex: Integer); virtual;

    property CharacterFormattingInfo: TdxCharacterFormattingInfo read FCharacterFormattingInfo;
    property CharacterFormattingOptions: TdxCharacterFormattingOptions read FCharacterFormattingOptions;
    property DocumentModel: TdxDocumentModel read FDocumentModel;
    property PictureBulletInformation: TdxDocPictureBulletInformation read FPictureBulletInformation write FPictureBulletInformation;
  public
    constructor Create(AOutput: TdxMemoryStream; AFontNameIndex: Integer); overload;
    constructor Create(AOutput: TdxMemoryStream; AProperties: TdxCharacterProperties; AFontNameIndex: Integer); overload;
    constructor Create(AOutput: TdxMemoryStream; AParagraphStyleInfo: TdxDocParagraphStyleInfo); overload;
    destructor Destroy; override;
    procedure CreateInlinePicturePropertiesModifiers(ACharacterStyleIndex: Integer; ADataStreamOffset: Integer);
    procedure CreateCharacterPropertiesModifiers; overload;
    procedure CreateCharacterPropertiesModifiers(ACharacterStyleIndex, AParagraphStyleIndex: Integer; ASpecialSymbol: Boolean); overload;
    procedure AllCapsAction;
    procedure BackColorAction;
    procedure ForeColorAction;
    procedure StrikeoutColorAction;
    procedure UnderlineColorAction;
    procedure FontBoldAction;
    procedure FontItalicAction;
    procedure FontNameAction;
    procedure FontSizeAction;
    procedure DoubleFontSizeAction;
    procedure FontStrikeoutTypeAction;
    procedure FontUnderlineTypeAction;
    procedure HiddenAction;
    procedure ScriptAction;
    procedure LangInfoAction;
    procedure NoProofAction;
    procedure StrikeoutWordsOnlyAction;
    procedure UnderlineWordsOnlyAction;
    procedure UnderlineAction;
    procedure WriteCharacterStyleIndex(ACharacterStyleIndex: Integer);
    procedure PictureBulletInformationAction;
    procedure PictureOffsetAction(ADataStreamOffset: Integer);
    procedure SpecialSymbolAction;
  end;

  { TdxDocListCharacterPropertiesActions }

  TdxDocListCharacterPropertiesActions = class(TdxDocCharacterPropertiesActions)
  protected
    procedure SetUseDefaultFontSize(ACharacterStyleIndex, AParagraphStyleIndex: Integer); override;
  public
    constructor Create(AOutput: TdxMemoryStream; AListLevel: TdxListLevel; AFontNameIndex: Integer);
  end;

implementation

uses
  Math, Contnrs,
  dxEncoding,
  dxStringHelper,
  dxRichEdit.Import.Doc.DocCommand;

{ TdxDocCharacterPropertiesActions }

constructor TdxDocCharacterPropertiesActions.Create(AOutput: TdxMemoryStream; AFontNameIndex: Integer);
begin
  Assert(AOutput <> nil, 'output');
  FWriter := TBinaryWriter.Create(AOutput);
  FFontNameIndex := AFontNameIndex;
end;

constructor TdxDocCharacterPropertiesActions.Create(AOutput: TdxMemoryStream; AProperties: TdxCharacterProperties; AFontNameIndex: Integer);
begin
  Create(AOutput, AFontNameIndex);
  if AProperties <> nil then
  begin
    FCharacterFormattingInfo := AProperties.Info.Info;
    FCharacterFormattingOptions := AProperties.Info.Options;
    FDocumentModel := TdxDocumentModel(AProperties.DocumentModel);
  end;
end;

constructor TdxDocCharacterPropertiesActions.Create(AOutput: TdxMemoryStream; AParagraphStyleInfo: TdxDocParagraphStyleInfo);
begin
  Assert(AOutput <> nil, 'output');
  Create(AOutput, AParagraphStyleInfo.FontNameIndex);
  FCharacterFormattingInfo := AParagraphStyleInfo.CharacterFormattingInfo;
  FCharacterFormattingOptions := AParagraphStyleInfo.CharacterFormattingOptions;
  FDocumentModel := AParagraphStyleInfo.DocumentModel;
end;

destructor TdxDocCharacterPropertiesActions.Destroy;
begin
  FPictureBulletInformation.Free;
  FWriter.Free;
  inherited Destroy;
end;

procedure TdxDocCharacterPropertiesActions.CreateInlinePicturePropertiesModifiers(ACharacterStyleIndex: Integer; ADataStreamOffset: Integer);
begin
  CreateCharacterPropertiesModifiers(ACharacterStyleIndex, TdxDocStyleIndexes.DefaultParagraphStyleIndex, True);
  PictureOffsetAction(ADataStreamOffset);
end;

procedure TdxDocCharacterPropertiesActions.CreateCharacterPropertiesModifiers;
begin
  CreateCharacterPropertiesModifiers(TdxDocStyleIndexes.DefaultCharacterStyleIndex, TdxDocStyleIndexes.DefaultParagraphStyleIndex, False);
end;

procedure TdxDocCharacterPropertiesActions.CreateCharacterPropertiesModifiers(ACharacterStyleIndex, AParagraphStyleIndex: Integer; ASpecialSymbol: Boolean);
begin
  SetUseDefaultFontSize(ACharacterStyleIndex, AParagraphStyleIndex);
  if CharacterFormattingInfo <> nil then
  begin
    AllCapsAction;
    BackColorAction;
    ForeColorAction;
    StrikeoutColorAction;
    UnderlineColorAction;
    FontBoldAction;
    FontItalicAction;
    FontNameAction;
    FontSizeAction;
    DoubleFontSizeAction;
    FontStrikeoutTypeAction;
    FontUnderlineTypeAction;
    HiddenAction;
    ScriptAction;
    StrikeoutWordsOnlyAction;
    UnderlineWordsOnlyAction;
    LangInfoAction;
    NoProofAction;
  end;
  if ASpecialSymbol then
    SpecialSymbolAction;
  PictureBulletInformationAction;
end;

procedure TdxDocCharacterPropertiesActions.SetUseDefaultFontSize(ACharacterStyleIndex: Integer; AParagraphStyleIndex: Integer);
begin
  if ACharacterStyleIndex <> TdxDocStyleIndexes.DefaultCharacterStyleIndex then
    WriteCharacterStyleIndex(ACharacterStyleIndex)
  else
    FUseDefaultFontSize := AParagraphStyleIndex = TdxDocStyleIndexes.DefaultParagraphStyleIndex;
end;

procedure TdxDocCharacterPropertiesActions.AllCapsAction;
var
  ACommand: TdxDocCommandAllCaps;
begin
  if not CharacterFormattingOptions.UseAllCaps then
    Exit;
  ACommand := TdxDocCommandAllCaps.Create;
  try
    ACommand.Value := CharacterFormattingInfo.AllCaps;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocCharacterPropertiesActions.BackColorAction;
var
  ACommand: TdxDocCommandBackColor;
begin
  if not CharacterFormattingOptions.UseBackColor or TdxAlphaColors.IsTransparentOrEmpty(CharacterFormattingInfo.BackColor) then
    Exit;
  ACommand := TdxDocCommandBackColor.Create;
  try
    ACommand.ShadingDescriptor.BackgroundColor := CharacterFormattingInfo.BackColor;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocCharacterPropertiesActions.ForeColorAction;
var
  ACommand: TdxDocCommandForeColor;
begin
  if not CharacterFormattingOptions.UseForeColor then
    Exit;
  ACommand := TdxDocCommandForeColor.Create;
  try
    ACommand.Color := CharacterFormattingInfo.ForeColor;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocCharacterPropertiesActions.StrikeoutColorAction;
begin
end;

procedure TdxDocCharacterPropertiesActions.UnderlineColorAction;
var
  ACommand: TdxDocCommandUnderlineColor;
begin
  if not CharacterFormattingOptions.UseUnderlineColor then
    Exit;
  ACommand := TdxDocCommandUnderlineColor.Create;
  try
    ACommand.Color := CharacterFormattingInfo.UnderlineColor;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocCharacterPropertiesActions.FontBoldAction;
var
  ACommand: TdxDocCommandBold;
begin
  if not CharacterFormattingOptions.UseFontBold then
    Exit;
  ACommand := TdxDocCommandBold.Create;
  try
    ACommand.Value := CharacterFormattingInfo.FontBold;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocCharacterPropertiesActions.FontItalicAction;
var
  ACommand: TdxDocCommandItalic;
begin
  if not CharacterFormattingOptions.UseFontItalic then
    Exit;
  ACommand := TdxDocCommandItalic.Create;
  try
    ACommand.Value := CharacterFormattingInfo.FontItalic;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocCharacterPropertiesActions.FontNameAction;
var
  AFontNameCommand: TdxDocCommandFontName;
  AEastAsianFontNameCommand: TdxDocCommandEastAsianFontName;
  ANonASCIIFontNameCommand: TdxDocCommandNonASCIIFontName;
begin
  if not CharacterFormattingOptions.UseFontName then
    Exit;

  AFontNameCommand := TdxDocCommandFontName.Create;
  try
    AFontNameCommand.Value := FFontNameIndex;
    AFontNameCommand.Write(FWriter);
  finally
    AFontNameCommand.Free;
  end;

  AEastAsianFontNameCommand := TdxDocCommandEastAsianFontName.Create;
  try
    AEastAsianFontNameCommand.Value := FFontNameIndex;
    AEastAsianFontNameCommand.Write(FWriter);
  finally
    AEastAsianFontNameCommand.Free;
  end;
  ANonASCIIFontNameCommand := TdxDocCommandNonASCIIFontName.Create;
  try
    ANonASCIIFontNameCommand.Value := FFontNameIndex;
    ANonASCIIFontNameCommand.Write(FWriter);
  finally
    ANonASCIIFontNameCommand.Free;
  end;
end;

procedure TdxDocCharacterPropertiesActions.FontSizeAction;
begin
end;

function TdxDocCharacterPropertiesActions.GetActualDoubleFontSize(AUseFontSize: Boolean): Integer;
var
  ADefaultStyleCharacterProperties: TdxCharacterProperties;
begin
  if AUseFontSize then
    Exit(CharacterFormattingInfo.DoubleFontSize);

  ADefaultStyleCharacterProperties := DocumentModel.ParagraphStyles.DefaultItem.CharacterProperties;
  if ADefaultStyleCharacterProperties.UseDoubleFontSize then
    Result := ADefaultStyleCharacterProperties.DoubleFontSize
  else
    Result := DocumentModel.DefaultCharacterProperties.DoubleFontSize;
end;

procedure TdxDocCharacterPropertiesActions.DoubleFontSizeAction;
var
  AUseFontSize: Boolean;
  ACommand: TdxDocCommandFontSize;
begin
  AUseFontSize := CharacterFormattingOptions.UseDoubleFontSize;
  if not AUseFontSize and not FUseDefaultFontSize then
    Exit;
  ACommand := TdxDocCommandFontSize.Create;
  try
    ACommand.DoubleFontSize := GetActualDoubleFontSize(AUseFontSize);
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocCharacterPropertiesActions.FontStrikeoutTypeAction;
var
  ACommandStrike: TdxDocCommandStrike;
  ACommandDoubleStrike: TdxDocCommandDoubleStrike;
begin
  if not CharacterFormattingOptions.UseFontStrikeoutType then
    Exit;
  ACommandStrike := TdxDocCommandStrike.Create;
  try
    ACommandDoubleStrike := TdxDocCommandDoubleStrike.Create;
    try
      if CharacterFormattingInfo.FontStrikeoutType = TdxStrikeoutType.Single then
      begin
        ACommandStrike.Value := True;
        ACommandStrike.Write(FWriter);
        ACommandDoubleStrike.Value := False;
        ACommandDoubleStrike.Write(FWriter);
      end
      else
        if CharacterFormattingInfo.FontStrikeoutType = TdxStrikeoutType.Double then
        begin
          ACommandStrike.Value := False;
          ACommandStrike.Write(FWriter);
          ACommandDoubleStrike.Value := True;
          ACommandDoubleStrike.Write(FWriter);
        end
        else
        begin
          ACommandStrike.Value := False;
          ACommandStrike.Write(FWriter);
          ACommandDoubleStrike.Value := False;
          ACommandDoubleStrike.Write(FWriter);
        end;
    finally
      ACommandDoubleStrike.Free;
    end;
  finally
    ACommandStrike.Free;
  end;
end;

procedure TdxDocCharacterPropertiesActions.FontUnderlineTypeAction;
begin
  if not CharacterFormattingOptions.UseFontUnderlineType then
    Exit;
  UnderlineAction;
end;

procedure TdxDocCharacterPropertiesActions.HiddenAction;
var
  ACommand: TdxDocCommandHidden;
begin
  if not CharacterFormattingOptions.UseHidden then
    Exit;
  ACommand := TdxDocCommandHidden.Create;
  try
    ACommand.Value := CharacterFormattingInfo.Hidden;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocCharacterPropertiesActions.ScriptAction;
var
  ACommand: TdxDocCommandScript;
begin
  if not CharacterFormattingOptions.UseScript then
    Exit;
  ACommand := TdxDocCommandScript.Create;
  try
    ACommand.Script := CharacterFormattingInfo.Script;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocCharacterPropertiesActions.LangInfoAction;
begin
end;

procedure TdxDocCharacterPropertiesActions.NoProofAction;
begin
end;

procedure TdxDocCharacterPropertiesActions.StrikeoutWordsOnlyAction;
begin
end;

procedure TdxDocCharacterPropertiesActions.UnderlineWordsOnlyAction;
begin
  if not CharacterFormattingOptions.UseUnderlineWordsOnly or
     not CharacterFormattingOptions.UseFontUnderlineType or
     (CharacterFormattingInfo.FontUnderlineType <> TdxUnderlineType.Single) then
    Exit;
  UnderlineAction;
end;

procedure TdxDocCharacterPropertiesActions.UnderlineAction;
var
  ACommand: TdxDocCommandUnderline;
begin
  ACommand := TdxDocCommandUnderline.Create;
  try
    ACommand.FontUnderlineType := CharacterFormattingInfo.FontUnderlineType;
    ACommand.UnderlyneWordsOnly := CharacterFormattingInfo.UnderlineWordsOnly;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocCharacterPropertiesActions.WriteCharacterStyleIndex(ACharacterStyleIndex: Integer);
var
  ACommand: TdxDocCommandChangeCharacterStyle;
begin
  ACommand := TdxDocCommandChangeCharacterStyle.Create;
  try
    ACommand.Value := ACharacterStyleIndex;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocCharacterPropertiesActions.PictureBulletInformationAction;
var
  ACharacterPositionCommand: TdxDocCommandPictureBulletCharacterPosition;
  ABulletProperties: TdxDocCommandPictureBulletProperties;
begin
  if FPictureBulletInformation = nil then
    Exit;
  ACharacterPositionCommand := TdxDocCommandPictureBulletCharacterPosition.Create;
  try
    ACharacterPositionCommand.Value := FPictureBulletInformation.PictureCharacterPosition;
    ACharacterPositionCommand.Write(FWriter);
  finally
    ACharacterPositionCommand.Free;
  end;
  ABulletProperties := TdxDocCommandPictureBulletProperties.Create;
  try
    ABulletProperties.DefaultPicture := FPictureBulletInformation.DefaultPicture;
    ABulletProperties.PictureBullet := FPictureBulletInformation.PictureBullet;
    ABulletProperties.SuppressBulletResize := FPictureBulletInformation.SuppressBulletResize;
    ABulletProperties.Write(FWriter);
  finally
    ABulletProperties.Free;
  end;
end;

procedure TdxDocCharacterPropertiesActions.PictureOffsetAction(ADataStreamOffset: Integer);
var
  APictureOffsetCommand: TdxDocCommandPictureOffset;
begin
  APictureOffsetCommand := TdxDocCommandPictureOffset.Create;
  try
    APictureOffsetCommand.Value := ADataStreamOffset;
    APictureOffsetCommand.Write(FWriter);
  finally
    APictureOffsetCommand.Free;
  end;
end;

procedure TdxDocCharacterPropertiesActions.SpecialSymbolAction;
var
  ACommand: TdxDocCommandSpecial;
begin
  ACommand := TdxDocCommandSpecial.Create;
  try
    ACommand.Value := True;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

{ TdxDocListCharacterPropertiesActions }

constructor TdxDocListCharacterPropertiesActions.Create(AOutput: TdxMemoryStream; AListLevel: TdxListLevel; AFontNameIndex: Integer);
begin
  inherited Create(AOutput, AFontNameIndex);
  FCharacterFormattingInfo := AListLevel.CharacterProperties.Info.Info;
  FCharacterFormattingOptions := AListLevel.CharacterProperties.Info.Options;
  PictureBulletInformation := TdxDocPictureBulletInformation.Create;
  PictureBulletInformation.SuppressBulletResize := AListLevel.ListLevelProperties.SuppressBulletResize;
end;

procedure TdxDocListCharacterPropertiesActions.SetUseDefaultFontSize(ACharacterStyleIndex, AParagraphStyleIndex: Integer);
begin
end;

end.
