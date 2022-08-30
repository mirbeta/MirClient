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

unit dxRichEdit.Import.Rtf.DestinationStyleSheet;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Classes, SysUtils, dxCore, dxCoreClasses,
  dxRichEdit.Import.Rtf,
  dxRichEdit.Import.Rtf.DestinationPieceTable,
  dxRichEdit.Import.Rtf.ParagraphFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Styles;

type
  TdxStyleSheetDestination = class(TdxDestinationPieceTable)
  strict private
    class var FKeywordHT: TdxKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateKeywordHT: TdxKeywordTranslatorTable; static;
  private
    FQFormat: Boolean;
    FStyleName: string;

    procedure AddParagraphStyleCollectionIndex(AInfo: TdxRtfParagraphFormattingInfo; AStyle: TdxParagraphStyle);
    function GetParagraphStyle(AInfo: TdxRtfParagraphFormattingInfo): TdxParagraphStyle;
    class function GetPrimaryStyleName(const AStyleName: string): string;

    // handlers
    class procedure CharacterStyleHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure NextStyleIndexHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ParagraphStyleHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ParentStyleIndexHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure StyleLinkKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure StyleListLevelHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure StyleListOverrideHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure StyleQFormatKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure TableStyleHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
  protected
    function CanAppendText: Boolean; override;
    function CreateClone: TdxRichEditRtfDestinationBase; override;
    class function GetControlCharHT: TdxControlCharTranslatorTable; override;
    class function GetKeywordHT: TdxKeywordTranslatorTable; override;
    procedure ProcessCharCore(AChar: Char); override;

    property QFormat: Boolean read FQFormat write FQFormat;
  public
    constructor Create(AImporter: TdxRtfImporter); reintroduce;
    procedure FinalizePieceTableCreation; override;
    procedure NestedGroupFinished(ADestination: TdxRichEditRtfDestinationBase); override;
  end;

  TdxDestinationCharacterStyle = class(TdxDestinationPieceTable)
  strict private
    class var FKeywordHT: TdxKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateKeywordHT: TdxKeywordTranslatorTable; static;
  private
    FStyleName: string;
    FQFormat: Boolean;

    function GetCharacterStyleByName(const AName: string): TdxCharacterStyle;
    // handlers
    class procedure ParentStyleIndexHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure StyleLinkKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure StyleQFormatKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
  protected
    function CanAppendText: Boolean; override;
    class function GetControlCharHT: TdxControlCharTranslatorTable; override;
    class function GetKeywordHT: TdxKeywordTranslatorTable; override;
    procedure ProcessCharCore(AChar: Char); override;

    property QFormat: Boolean read FQFormat write FQFormat;
  public
    constructor Create(AImporter: TdxRtfImporter; AStyleIndex: Integer); reintroduce;
    procedure BeforePopRtfState; override;
    procedure FinalizePieceTableCreation; override;
  end;

implementation

uses
  Graphics, dxCoreGraphics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.FrameFormatting,
  dxRichEdit.Import.Rtf.DestinationTableStyle;

{ TdxStyleSheetDestination }

procedure TdxStyleSheetDestination.NestedGroupFinished(ADestination: TdxRichEditRtfDestinationBase);
var
  AStyleSheetDestination: TdxStyleSheetDestination;
  AParagraphFormattingInfo: TdxRtfParagraphFormattingInfo;
  AStyle: TdxParagraphStyle;
  AName: string;
  AParentCharacterProperties: TdxMergedCharacterProperties;
  AIndex, ALevelIndex: Integer;
begin
  AStyleSheetDestination := Safe<TdxStyleSheetDestination>.Cast(ADestination);
  if AStyleSheetDestination <> nil then
  begin
    AParagraphFormattingInfo := Importer.Position.ParagraphFormattingInfo;
    AStyle := GetParagraphStyle(AParagraphFormattingInfo);
    try
      AName := GetPrimaryStyleName(AStyleSheetDestination.FStyleName);
      if AName <> '' then
        AStyle.StyleName := AName;
      if (AParagraphFormattingInfo.StyleLink >= 0) and not Importer.LinkParagraphStyleIndexToCharacterStyleIndex.ContainsKey(AParagraphFormattingInfo.StyleIndex) then
        Importer.LinkParagraphStyleIndexToCharacterStyleIndex.Add(Importer.Position.ParagraphFormattingInfo.StyleIndex, AParagraphFormattingInfo.StyleLink);
      if AParagraphFormattingInfo.NextStyle >= 0 then
        Importer.NextParagraphStyleIndexTable.AddOrSetValue(AParagraphFormattingInfo.StyleIndex, AParagraphFormattingInfo.NextStyle);

      AParentCharacterProperties := Importer.GetStyleMergedParagraphCharacterProperties(-1, AParagraphFormattingInfo.ParentStyleIndex);
      try
        Importer.ApplyParagraphProperties(AStyle.ParagraphProperties, AParagraphFormattingInfo.ParentStyleIndex, AParagraphFormattingInfo);
        Importer.ApplyCharacterProperties(AStyle.CharacterProperties, Importer.Position.CharacterFormatting.Info, AParentCharacterProperties);
        Importer.ApplyTabs(AStyle.Tabs, AParagraphFormattingInfo.ParentStyleIndex, AParagraphFormattingInfo.Tabs);
      finally
        AParentCharacterProperties.Free;
      end;
      if Importer.Position.ParagraphFrameFormattingInfo <> nil then
      begin
        AStyle.FrameProperties := TdxFrameProperties.Create(AStyle);
        Importer.ApplyParagraphFrameFormatting(AStyle.FrameProperties, Importer.Position.ParagraphFrameFormattingInfo);
      end;
      if AParagraphFormattingInfo.NumberingListIndex >= 0 then
      begin
        if not Importer.ParagraphStyleListOverrideIndexMap.ContainsKey(AStyle) then
        begin
          AIndex := AParagraphFormattingInfo.NumberingListIndex;
          ALevelIndex := AParagraphFormattingInfo.ListLevelIndex;
          Importer.ParagraphStyleListOverrideIndexMap.Add(AStyle, TdxRtfNumberingListInfo.Create(AIndex, ALevelIndex));
        end;
      end;
      AStyle.Primary := AStyleSheetDestination.QFormat;
      QFormat := False;
      AddParagraphStyleCollectionIndex(AParagraphFormattingInfo, AStyle);
    finally
      if Importer.DocumentModel.ParagraphStyles.IndexOf(AStyle) = -1 then
        AStyle.Free;
    end;
  end;
end;

procedure TdxStyleSheetDestination.FinalizePieceTableCreation;
begin
//do nothing
end;

function TdxStyleSheetDestination.CanAppendText: Boolean;
begin
  Result := False;
end;

procedure TdxStyleSheetDestination.ProcessCharCore(AChar: Char);
begin
  if AChar <> ';' then
    FStyleName := FStyleName + AChar;
end;

procedure TdxStyleSheetDestination.AddParagraphStyleCollectionIndex(AInfo: TdxRtfParagraphFormattingInfo;
  AStyle: TdxParagraphStyle);
var
  AParagraphStyleIndex: Integer;
begin
  if not Importer.ParagraphStyleCollectionIndex.ContainsKey(AInfo.StyleIndex) then
  begin
    Importer.DocumentModel.ParagraphStyles.Add(AStyle);
    if (Importer.ParagraphStyleCollectionIndex.ContainsKey(AInfo.ParentStyleIndex)) then
    begin
      AParagraphStyleIndex := Importer.ParagraphStyleCollectionIndex[AInfo.ParentStyleIndex];
      AStyle.Parent := Importer.DocumentModel.ParagraphStyles[AParagraphStyleIndex];
    end;
    Importer.ParagraphStyleCollectionIndex.Add(AInfo.StyleIndex, Importer.DocumentModel.ParagraphStyles.Count - 1);
  end;
end;

function TdxStyleSheetDestination.GetParagraphStyle(AInfo: TdxRtfParagraphFormattingInfo): TdxParagraphStyle;
begin
  if AInfo.StyleIndex = 0 then
    Result := Importer.DocumentModel.ParagraphStyles[0]
  else
    Result := TdxParagraphStyle.Create(Importer.DocumentModel, nil);
end;

class function TdxStyleSheetDestination.GetPrimaryStyleName(const AStyleName: string): string;
var
  AStringList: TStringList;
  I: Integer;
  AName: string;
begin
  Result := '';
  AStringList := TStringList.Create;
  try
    ExtractStrings([','], [' '], PChar(AStyleName), AStringList);
    for I := 0 to AStringList.Count - 1 do
    begin
      AName := Trim(AStringList[I]);
      if AName <> '' then
      begin
        Result := AName;
        Break;
      end;
    end;
  finally
    AStringList.Free;
  end;
end;

class procedure TdxStyleSheetDestination.CharacterStyleHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxDestinationCharacterStyle.Create(AImporter, AParameterValue);
end;

constructor TdxStyleSheetDestination.Create(AImporter: TdxRtfImporter);
begin
  inherited Create(AImporter, AImporter.PieceTable);
end;

function TdxStyleSheetDestination.CreateClone: TdxRichEditRtfDestinationBase;
begin
  Result := TdxStyleSheetDestination.Create(Importer);
  TdxStyleSheetDestination(Result).FStyleName := FStyleName;
end;

class function TdxStyleSheetDestination.GetControlCharHT: TdxControlCharTranslatorTable;
begin
  Result := nil;
end;

class function TdxStyleSheetDestination.GetKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := FKeywordHT;
end;

class constructor TdxStyleSheetDestination.Initialize;
begin
  FKeywordHT := CreateKeywordHT;
end;

class destructor TdxStyleSheetDestination.Finalize;
begin
  FreeAndNil(FKeywordHT);
end;

class function TdxStyleSheetDestination.CreateKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := TdxKeywordTranslatorTable.Create;
  Result.Add('s', ParagraphStyleHandler);
  Result.Add('sqformat', StyleQFormatKeywordHandler);
  Result.Add('sbasedon', ParentStyleIndexHandler);
  Result.Add('cs', CharacterStyleHandler);
  Result.Add('ts', TableStyleHandler);
  Result.Add('slink', StyleLinkKeywordHandler);
  Result.Add('snext', NextStyleIndexHandler);
  Result.Add('ls', StyleListOverrideHandler);
  Result.Add('ilvl', StyleListLevelHandler);
  AddParagraphPropertiesKeywords(Result);
  AddCharacterPropertiesKeywords(Result);
  AddCommonTabKeywords(Result);
end;

class procedure TdxStyleSheetDestination.NextStyleIndexHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.ParagraphFormattingInfo.NextStyle := AParameterValue;
end;

class procedure TdxStyleSheetDestination.ParagraphStyleHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.ParagraphFormattingInfo.StyleIndex := AParameterValue;
end;

class procedure TdxStyleSheetDestination.ParentStyleIndexHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.ParagraphFormattingInfo.ParentStyleIndex := AParameterValue;
end;

class procedure TdxStyleSheetDestination.StyleLinkKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.ParagraphFormattingInfo.StyleLink := AParameterValue;
end;

class procedure TdxStyleSheetDestination.StyleListLevelHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.ParagraphFormattingInfo.ListLevelIndex := AParameterValue;
end;

class procedure TdxStyleSheetDestination.StyleListOverrideHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.ParagraphFormattingInfo.NumberingListIndex := AParameterValue;
end;

class procedure TdxStyleSheetDestination.StyleQFormatKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  TdxStyleSheetDestination(AImporter.Destination).QFormat := True;
end;

class procedure TdxStyleSheetDestination.TableStyleHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxDestinationTableStyle.Create(AImporter, AParameterValue);
end;

{ TdxDestinationCharacterStyle }

constructor TdxDestinationCharacterStyle.Create(
  AImporter: TdxRtfImporter; AStyleIndex: Integer);
begin
  inherited Create(AImporter, AImporter.PieceTable);
  AImporter.Position.CharacterStyleIndex := AStyleIndex;
end;

class constructor TdxDestinationCharacterStyle.Initialize;
begin
  FKeywordHT := CreateKeywordHT;
end;

class destructor TdxDestinationCharacterStyle.Finalize;
begin
  FreeAndNil(FKeywordHT);
end;

class function TdxDestinationCharacterStyle.CreateKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := TdxKeywordTranslatorTable.Create;
  Result.Add('sbasedon', ParentStyleIndexHandler);
  Result.Add('slink', StyleLinkKeywordHandler);
  Result.Add('sqformat', StyleQFormatKeywordHandler);
  AddCharacterPropertiesKeywords(Result);
end;

procedure TdxDestinationCharacterStyle.FinalizePieceTableCreation;
begin
//do nothing
end;

procedure TdxDestinationCharacterStyle.BeforePopRtfState;
var
  AName: string;
  AStyle: TdxCharacterStyle;
  AParentCharacterProperties: TdxMergedCharacterProperties;
begin
  AName := TdxStyleSheetDestination.GetPrimaryStyleName(FStyleName);
  if not Importer.CharacterStyleCollectionIndex.ContainsKey(Importer.Position.CharacterStyleIndex) then
  begin
    AStyle := GetCharacterStyleByName(AName);
    AStyle.Primary := QFormat;
    if (Importer.Position.ParagraphFormattingInfo.StyleLink >= 0) and
        not Importer.LinkParagraphStyleIndexToCharacterStyleIndex.ContainsKey(Importer.Position.ParagraphFormattingInfo.StyleLink) then
      Importer.LinkParagraphStyleIndexToCharacterStyleIndex.Add(Importer.Position.ParagraphFormattingInfo.StyleLink, Importer.Position.CharacterStyleIndex);
    if AName <> TdxCharacterStyleCollection.DefaultCharacterStyleName then
    begin
      AParentCharacterProperties := Importer.GetStyleMergedCharacterProperties(Importer.Position.RtfFormattingInfo.ParentStyleIndex);
      try
        Importer.ApplyCharacterProperties(AStyle.CharacterProperties, Importer.Position.CharacterFormatting.Info, AParentCharacterProperties);
      finally
        AParentCharacterProperties.Free;
      end;
    end
    else
    if Importer.DocumentModel.ShouldApplyAppearanceProperties then
    begin
      AStyle.CharacterProperties.BeginUpdate;
      try
        if Importer.Position.CharacterFormatting.FontName <> AStyle.CharacterProperties.FontName then
          AStyle.CharacterProperties.FontName := Importer.Position.CharacterFormatting.FontName;
        if Importer.Position.CharacterFormatting.DoubleFontSize <> AStyle.CharacterProperties.DoubleFontSize then
          AStyle.CharacterProperties.DoubleFontSize := Importer.Position.CharacterFormatting.DoubleFontSize;
        if Importer.Position.CharacterFormatting.ForeColor <> AStyle.CharacterProperties.ForeColor then
          AStyle.CharacterProperties.ForeColor := Importer.Position.CharacterFormatting.ForeColor
        else
          AStyle.CharacterProperties.ForeColor := Importer.DocumentProperties.Colors[0];
      finally
        AStyle.CharacterProperties.EndUpdate;
      end;
    end;
    if Importer.CharacterStyleCollectionIndex.ContainsKey(Importer.Position.RtfFormattingInfo.ParentStyleIndex) then
      AStyle.Parent := Importer.DocumentModel.CharacterStyles[Importer.CharacterStyleCollectionIndex[Importer.Position.RtfFormattingInfo.ParentStyleIndex]];
  end;
end;

function TdxDestinationCharacterStyle.CanAppendText: Boolean;
begin
  Result := False;
end;

class function TdxDestinationCharacterStyle.GetControlCharHT: TdxControlCharTranslatorTable;
begin
  Result := nil;
end;

class function TdxDestinationCharacterStyle.GetKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := FKeywordHT;
end;

procedure TdxDestinationCharacterStyle.ProcessCharCore(AChar: Char);
begin
  if AChar <> ';' then
    FStyleName := FStyleName + AChar;
end;

function TdxDestinationCharacterStyle.GetCharacterStyleByName(const AName: string): TdxCharacterStyle;
var
  ADomumentModel: TdxDocumentModel;
  ACharacterStyles: TdxCharacterStyleCollection;
  AStyleIndex: Integer;
begin
  ADomumentModel := Importer.DocumentModel;
  ACharacterStyles := ADomumentModel.CharacterStyles;
  AStyleIndex := ACharacterStyles.GetStyleIndexByName(AName);
  if AStyleIndex >= 0 then
  begin
    if not Importer.CharacterStyleCollectionIndex.ContainsKey(Importer.Position.CharacterStyleIndex) then
      Importer.CharacterStyleCollectionIndex.Add(Importer.Position.CharacterStyleIndex, AStyleIndex)
    else
      Importer.CharacterStyleCollectionIndex[Importer.Position.CharacterStyleIndex] := AStyleIndex;
    Result := ACharacterStyles[AStyleIndex];
  end
  else
  begin
    Result := TdxCharacterStyle.Create(ADomumentModel, nil);
    Result.StyleName := AName;
    AStyleIndex := ACharacterStyles.Add(Result);
    Importer.CharacterStyleCollectionIndex.Add(Importer.Position.CharacterStyleIndex, AStyleIndex);
  end;
end;

class procedure TdxDestinationCharacterStyle.ParentStyleIndexHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.RtfFormattingInfo.ParentStyleIndex := AParameterValue;
end;

class procedure TdxDestinationCharacterStyle.StyleLinkKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.ParagraphFormattingInfo.StyleLink := AParameterValue;
end;

class procedure TdxDestinationCharacterStyle.StyleQFormatKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  TdxDestinationCharacterStyle(AImporter.Destination).QFormat := True;
  AImporter.Position.RtfFormattingInfo.ParentStyleIndex := AParameterValue;
end;

end.
