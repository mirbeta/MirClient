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

unit dxRichEdit.DocumentModel.TextRange;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Classes, SysUtils, Graphics, Generics.Defaults, Generics.Collections,

  dxRichEdit.Utils.ChunkedStringBuilder,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.Boxes.Core,
  dxRichEdit.DocumentModel.CopyManager.Core,
  dxRichEdit.DocumentModel.CopyManager.Simple,
  dxRichEdit.DocumentModel.CharacterFormatting;

type
  { TdxTextRun }

  TdxTextRun = class(TdxTextRunBase, IdxHighlightableTextRun)
  strict protected
    function IsFieldRun: Boolean; virtual;
  protected
    procedure CopyContentCore(ACopyManager: TdxSimpleDocumentModelCopyManager); virtual;
    procedure CopyCore(ATargetModel: TdxSimpleDocumentModel; ATargetRun: TdxTextRunBase); virtual;
  public
    function Copy(ACopyManager: TdxCustomDocumentModelCopyManager): TdxTextRunBase; override;
    function CreateRun(AParagraph: TdxSimpleParagraph; AStartIndex, ALength: Integer): TdxTextRun; virtual;
    function CanJoinWith(ANextRun: TdxTextRunBase): Boolean; override;
    function CanPlaceCaretBefore: Boolean; override;
    function GetText(ABuffer: TdxChunkedStringBuilder; AFrom, ATo: Integer): string; override;
    procedure Measure(ABoxInfo: TdxBoxInfo; const AMeasurer: IdxObjectMeasurer); override;
    function MatchFormatting(AFormatting: TdxCharacterFormattingInfo;
      const AOptions: TdxCharacterFormattingOptions; AStyleIndex: Integer): Boolean; override;
    function TryAdjustEndPositionToFit(ABoxInfo: TdxBoxInfo; AMaxWidth: Integer; const AMeasurer: IdxObjectMeasurer): Boolean; override;
    function IsFieldSymbolRun: Boolean; virtual;
  end;

  { TdxSpecialTextRun }

  TdxSpecialTextRun = class abstract(TdxTextRun)
  protected
    function GetRowProcessingFlags: TdxRowProcessingFlags; override;
    procedure SetRowProcessingFlags(const AValue: TdxRowProcessingFlags); override;
  end;

  { TdxSeparatorTextRun }

  TdxSeparatorTextRun = class(TdxTextRun)
  public const
    SeparatorText: string = '|';
  protected
    function GetText: string; override;
    function GetRawTextFast(ABuffer: TdxChunkedStringBuilder): string; override;
  public
    constructor Create(AParagraph: TdxParagraphBase); reintroduce;
    function CanJoinWith(ARun: TdxTextRunBase): Boolean; override;
    function CanPlaceCaretBefore: Boolean; override;
    function Copy(ACopyManager: TdxCustomDocumentModelCopyManager): TdxTextRunBase; override;
    function GetPlainText(ABuffer: TdxChunkedStringBuilder; AFrom, ATo: Integer): string; override;
    function GetTextFast(ABuffer: TdxChunkedStringBuilder): string; override;
    procedure Measure(ABoxInfo: TdxBoxInfo; const AMeasurer: IdxObjectMeasurer); override;
    function TryAdjustEndPositionToFit(ABoxInfo: TdxBoxInfo; AMaxWidth: Integer; const AMeasurer: IdxObjectMeasurer): Boolean; override;
  end;

implementation

uses
  RTLConsts, dxCoreClasses, dxTypeHelpers,

  dxRichEdit.Options.Core,
  dxRichEdit.Utils.Exceptions,
  dxCharacters,
  dxRichEdit.Platform.Font;


{ TdxTextRun }

function TdxTextRun.CanJoinWith(ANextRun: TdxTextRunBase): Boolean;
begin
  if ANextRun = nil then
    TdxRichEditExceptions.ThrowArgumentException('nextRun', ANextRun);
  Result := (ANextRun is TdxTextRun) and not TdxTextRun(ANextRun).IsFieldRun and
    (Paragraph = ANextRun.Paragraph) and (StartIndex + Length = ANextRun.StartIndex) and
    (CharacterStyleIndex = ANextRun.CharacterStyleIndex) and
    (CharacterProperties.Index = ANextRun.CharacterProperties.Index);
end;

function TdxTextRun.CanPlaceCaretBefore: Boolean;
begin
  Result := True;
end;

function TdxTextRun.Copy(ACopyManager: TdxCustomDocumentModelCopyManager): TdxTextRunBase;
var
  ATargetPieceTable: TdxSimplePieceTable;
  ATargetPosition: TdxDocumentModelPosition;
  ARun: TdxTextRunBase;
begin
  ATargetPieceTable := TdxSimplePieceTable(ACopyManager.TargetPieceTable);
  ATargetPosition := ACopyManager.TargetPosition;
  CopyContentCore(TdxSimpleDocumentModelCopyManager(ACopyManager));
  ARun := ATargetPieceTable.Runs[ATargetPosition.RunIndex];
  CopyCore(TdxSimpleDocumentModel(ACopyManager.TargetModel), ARun);
  Result := ARun;
end;

function TdxTextRun.GetText(ABuffer: TdxChunkedStringBuilder; AFrom, ATo: Integer): string;
var
  AText: string;
begin
  AText := inherited GetText(ABuffer, AFrom, ATo);
  if AllCaps then
    AText := AnsiUpperCase(AText);
  Result := AText;
end;

function TdxTextRun.CreateRun(AParagraph: TdxSimpleParagraph; AStartIndex, ALength: Integer): TdxTextRun;
begin
  Result := TdxTextRun.Create(AParagraph, AStartIndex, ALength);
end;

procedure TdxTextRun.Measure(ABoxInfo: TdxBoxInfo; const AMeasurer: IdxObjectMeasurer);
var
  AText: string;
  AFontInfo: TdxFontInfo;
begin
  AText := PieceTable.GetTextFromSingleRun(ABoxInfo.StartPos, ABoxInfo.EndPos);
  AFontInfo := DocumentModel.FontCache[FontCacheIndex];
  AMeasurer.MeasureText(ABoxInfo, AText, AFontInfo);
end;

function TdxTextRun.MatchFormatting(AFormatting: TdxCharacterFormattingInfo;
  const AOptions: TdxCharacterFormattingOptions; AStyleIndex: Integer): Boolean;
var
  AProperties: TdxCharacterProperties;
  AFormattingBaseInfo: TdxCharacterFormattingBase;
  ARunOptions: TdxCharacterFormattingOptions;
  ARunInfo: TdxCharacterFormattingInfo;
begin
  Result := CharacterStyleIndex = AStyleIndex;
  if not Result then
    Exit;
  AProperties := CharacterProperties;
  AFormattingBaseInfo := AProperties.Info;
  ARunOptions := AFormattingBaseInfo.Options;
  Result := AOptions = ARunOptions;
  if Result then
  begin
    ARunInfo := AFormattingBaseInfo.Info;
    Result :=
      (not ARunOptions.UseDoubleFontSize or (ARunInfo.DoubleFontSize = AFormatting.DoubleFontSize)) and
      (not ARunOptions.UseFontBold or (ARunInfo.FontBold = AFormatting.FontBold)) and
      (not ARunOptions.UseFontItalic or (ARunInfo.FontItalic = AFormatting.FontItalic)) and
      (not ARunOptions.UseFontStrikeoutType or (ARunInfo.FontStrikeoutType = AFormatting.FontStrikeoutType)) and
      (not ARunOptions.UseFontUnderlineType or (ARunInfo.FontUnderlineType = AFormatting.FontUnderlineType)) and
      (not ARunOptions.UseAllCaps or (ARunInfo.AllCaps = AFormatting.AllCaps)) and
      (not ARunOptions.UseHidden or (ARunInfo.Hidden = AFormatting.Hidden)) and
      (not ARunOptions.UseStrikeoutWordsOnly or (ARunInfo.StrikeoutWordsOnly = AFormatting.StrikeoutWordsOnly)) and
      (not ARunOptions.UseUnderlineWordsOnly or (ARunInfo.UnderlineWordsOnly = AFormatting.UnderlineWordsOnly)) and
      (not ARunOptions.UseForeColor or (ARunInfo.ForeColor = AFormatting.ForeColor)) and
      (not ARunOptions.UseBackColor or (ARunInfo.BackColor = AFormatting.BackColor)) and
      (not ARunOptions.UseUnderlineColor or (ARunInfo.UnderlineColor = AFormatting.UnderlineColor)) and
      (not ARunOptions.UseStrikeoutColor or (ARunInfo.StrikeoutColor = AFormatting.StrikeoutColor)) and
      (not ARunOptions.UseScript or (ARunInfo.Script = AFormatting.Script)) and
      (not ARunOptions.UseNoProof or (ARunInfo.NoProof = AFormatting.NoProof)) and
      (not ARunOptions.UseFontName or (ARunInfo.FontName = AFormatting.FontName));
  end;
end;

procedure TdxTextRun.CopyContentCore(ACopyManager: TdxSimpleDocumentModelCopyManager);
var
  ALogPosition: TdxDocumentLogPosition;
begin
   ALogPosition := ACopyManager.TargetPosition.LogPosition;
  if ACopyManager.TargetPieceTable <> ACopyManager.TargetModel.MainPieceTable then
    ACopyManager.TargetPieceTable.InsertText(ALogPosition,
      StringReplace(GetTextFast(ACopyManager.SourcePieceTable.TextBuffer), TdxCharacters.PageBreak, TdxCharacters.LineBreak, [rfReplaceAll]))
  else
    ACopyManager.TargetPieceTable.InsertText(ALogPosition, GetTextFast(ACopyManager.SourcePieceTable.TextBuffer));
end;

procedure TdxTextRun.CopyCore(ATargetModel: TdxSimpleDocumentModel; ATargetRun: TdxTextRunBase);
var
  AOptions: TdxCustomDocumentCapabilitiesOptions;
begin
  AOptions := ATargetModel.DocumentCapabilities;
  if AOptions.CharacterFormattingAllowed then
    ATargetRun.CharacterProperties.CopyFrom(CharacterProperties.Info);

  if AOptions.CharacterStyleAllowed then
    ATargetRun.CharacterStyleIndex := CharacterStyle.Copy(ATargetModel);
end;

function TdxTextRun.IsFieldRun: Boolean;
begin
  Result := False;
end;

function TdxTextRun.TryAdjustEndPositionToFit(ABoxInfo: TdxBoxInfo; AMaxWidth: Integer; const AMeasurer: IdxObjectMeasurer): Boolean;
var
  AText: string;
  AFontInfo: TdxFontInfo;
begin
  AText := PieceTable.GetTextFromSingleRun(ABoxInfo.StartPos, ABoxInfo.EndPos);
  AFontInfo := DocumentModel.FontCache[FontCacheIndex];
  Result := AMeasurer.TryAdjustEndPositionToFit(ABoxInfo, AText, AFontInfo, AMaxWidth);
end;

function TdxTextRun.IsFieldSymbolRun: Boolean;
begin
  Result := False;
end;

{ TdxSpecialTextRun }

function TdxSpecialTextRun.GetRowProcessingFlags: TdxRowProcessingFlags;
begin
  Result := inherited GetRowProcessingFlags;
  Include(Result, TdxRowProcessingFlag.ProcessSpecialTextBoxes)
end;

procedure TdxSpecialTextRun.SetRowProcessingFlags(const AValue: TdxRowProcessingFlags);
begin
  inherited SetRowProcessingFlags(AValue);
end;

{ TdxSeparatorTextRun }

constructor TdxSeparatorTextRun.Create(AParagraph: TdxParagraphBase);
begin
  inherited Create(AParagraph);
  FLength := 1;
end;

function TdxSeparatorTextRun.CanPlaceCaretBefore: Boolean;
begin
  Result := True;
end;

function TdxSeparatorTextRun.GetText: string;
begin
  Result := '';
end;

function TdxSeparatorTextRun.GetTextFast(ABuffer: TdxChunkedStringBuilder): string;
begin
  Result := '';
end;

function TdxSeparatorTextRun.GetRawTextFast(ABuffer: TdxChunkedStringBuilder): string;
begin
  Result := SeparatorText;
end;

function TdxSeparatorTextRun.GetPlainText(ABuffer: TdxChunkedStringBuilder; AFrom, ATo: Integer): string;
begin
  Result := GetPlainText(ABuffer);
end;

function TdxSeparatorTextRun.CanJoinWith(ARun: TdxTextRunBase): Boolean;
begin
  Result := False;
end;

procedure TdxSeparatorTextRun.Measure(ABoxInfo: TdxBoxInfo; const AMeasurer: IdxObjectMeasurer);
begin

  ABoxInfo.Size := TSize.Create(0, ABoxInfo.Size.Height);
end;

function TdxSeparatorTextRun.Copy(ACopyManager: TdxCustomDocumentModelCopyManager): TdxTextRunBase;
var
  ARun: TdxTextRunBase;
  ATargetPosition: TdxDocumentModelPosition;
  APieceTable: TdxSimplePieceTable;
begin
  APieceTable := TdxSimplePieceTable(ACopyManager.TargetPieceTable);
  ATargetPosition := ACopyManager.TargetPosition;
  APieceTable.InsertSeparatorTextRunCore(ATargetPosition.ParagraphIndex, ATargetPosition.LogPosition);
  ARun := APieceTable.Runs[ATargetPosition.RunIndex];
  Result := ARun;
  NotImplemented;
end;

function TdxSeparatorTextRun.TryAdjustEndPositionToFit(ABoxInfo: TdxBoxInfo; AMaxWidth: Integer; const AMeasurer: IdxObjectMeasurer): Boolean;
begin
  Result := False;
end;

end.
