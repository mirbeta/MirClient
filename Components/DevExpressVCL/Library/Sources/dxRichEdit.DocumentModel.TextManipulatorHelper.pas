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

unit dxRichEdit.DocumentModel.TextManipulatorHelper;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Graphics, StrUtils, dxCoreGraphics,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Simple,
  dxStringHelper;

type
  { TdxTextManipulatorHelper }

  TdxTextManipulatorHelper = class
  public
    class procedure SetText(ATable: TdxPieceTable; const AText: string); overload; static; inline;
    class procedure SetText(ATable: TdxPieceTable; const AText, ADefaultFontName: string;
      ADoubleFontSize: Integer); overload; static; inline;
    class function GetPlainTextLines(const AText: string): TArray<string>; static; inline;
    class procedure SetTextCore(ATable: TdxPieceTable; const AText: string); static; inline;
    class procedure SetTextForeColor(ATable: TdxPieceTable; AForeColor: TdxAlphaColor); static; inline;
    class procedure SetTextLines(ATable: TdxPieceTable; const ALines: TArray<string>); overload; static; inline;
    class procedure SetTextLines(ATable: TdxPieceTable; const ALines: TArray<string>; const ADefaultFontName: string;
      ADoubleFontSize: Integer); overload; static; inline;
    class procedure SetTextLinesCore(ATable: TdxPieceTable; const ALines: TArray<string>); static; inline;
    class procedure InsertTextLine(ATable: TdxPieceTable; APos: TdxInputPosition;
      const AText: string); static; inline;
  end;


implementation

uses
  dxRichEdit.DocumentModel.TextRange,
  dxRichEdit.Utils.Types,
  dxCharacters;

{ TdxTextManipulatorHelper }

class procedure TdxTextManipulatorHelper.SetText(ATable: TdxPieceTable; const AText: string);
begin
  SetText(ATable, AText, '', 0);
end;

class procedure TdxTextManipulatorHelper.SetText(ATable: TdxPieceTable; const AText: string; const ADefaultFontName: string; ADoubleFontSize: Integer);
var
  ADocumentModel: TdxDocumentModel;
begin
  ADocumentModel := ATable.DocumentModel;
  ADocumentModel.BeginSetContent;
  try
    if ADefaultFontName <> '' then
      ADocumentModel.DefaultCharacterProperties.FontName := ADefaultFontName;
    if ADoubleFontSize > 0 then
      ADocumentModel.DefaultCharacterProperties.DoubleFontSize := ADoubleFontSize;
    SetTextCore(ATable, AText);
  finally
    ADocumentModel.EndSetContent(TdxDocumentModelChangeType.LoadNewDocument, False, nil);
  end;
end;

class function TdxTextManipulatorHelper.GetPlainTextLines(const AText: string): TArray<string>;
var
  AIndex, L: Integer;
  AInput, AOutput, ALine: PChar;
  ABuffer: string;
  C: Char;
begin
  L := Length(AText);
  if L = 0 then
    Exit(TArray<string>.Create(''));
  SetLength(Result, L);
  SetLength(ABuffer, L);
  AIndex := 0;
  AInput := PChar(AText);
  AOutput := PChar(ABuffer);
  ALine  := AOutput;
  while AInput^ <> #$0000 do
  begin
    C := AInput^;
    case C of
      TdxCharacters.NonBreakingSpace:
        AOutput^ := TdxCharacters.Space;
      #13:
        begin
          if AInput[1] = #10 then
            Inc(AInput);
          SetString(Result[AIndex], ALine, AOutput - ALine);
          ALine := AOutput;
          Inc(ALine);
          Inc(AIndex);
        end;
      #10:
        begin
          if AInput[1] = #13 then
            Inc(AInput);
          SetString(Result[AIndex], ALine, AOutput - ALine);
          ALine := AOutput;
          Inc(ALine);
          Inc(AIndex);
        end;
    else
      AOutput^ := C;
    end;
    Inc(AInput);
    Inc(AOutput);
  end;
  if AOutput > ALine then
  begin
    SetString(Result[AIndex], ALine, AOutput - ALine);
    Inc(AIndex);
  end;
  SetLength(Result, AIndex);
end;

class procedure TdxTextManipulatorHelper.SetTextCore(ATable: TdxPieceTable; const AText: string);
var
  ALines: TArray<string>;
  S: string;
begin
  S := ReplaceStr(AText, TdxCharacters.SectionMark, TdxCharacters.ParagraphMark);
  ALines := GetPlainTextLines(S);
  SetTextLinesCore(ATable, ALines);
end;

class procedure TdxTextManipulatorHelper.SetTextForeColor(ATable: TdxPieceTable; AForeColor: TdxAlphaColor);
var
  ADocumentModel: TdxDocumentModel;
  ARuns: TdxTextRunCollection;
  ARunCount, I: TdxRunIndex;
begin
  ADocumentModel := ATable.DocumentModel;
  ADocumentModel.History.Clear;
  ADocumentModel.SwitchToEmptyHistory(True);
  try
    ARuns := ATable.Runs;
    ARunCount := ARuns.Count;
    for I := 0 to ARunCount - 1 do
    begin
      if ARuns[I] is TdxTextRun then
        TdxTextRun(ARuns[I]).ForeColor := AForeColor;
    end;
  finally
    ADocumentModel.SwitchToNormalHistory(True);
  end;
end;

class procedure TdxTextManipulatorHelper.SetTextLines(ATable: TdxPieceTable; const ALines: TArray<string>);
begin
  SetTextLines(ATable, ALines, '', 0);
end;

class procedure TdxTextManipulatorHelper.SetTextLines(ATable: TdxPieceTable; const ALines: TArray<string>; const ADefaultFontName: string; ADoubleFontSize: Integer);
var
  ADocumentModel: TdxDocumentModel;
begin
  ADocumentModel := ATable.DocumentModel;
  ADocumentModel.BeginSetContent;
  try
    if ADefaultFontName <> '' then
      ADocumentModel.DefaultCharacterProperties.FontName := ADefaultFontName;
    if ADoubleFontSize > 0 then
      ADocumentModel.DefaultCharacterProperties.DoubleFontSize := ADoubleFontSize;
    SetTextLinesCore(ATable, ALines);
  finally
    ADocumentModel.EndSetContent(TdxDocumentModelChangeType.LoadNewDocument, False, nil);
  end;
end;

class procedure TdxTextManipulatorHelper.SetTextLinesCore(ATable: TdxPieceTable; const ALines: TArray<string>);
var
  ACount, AMaxIndex, I: Integer;
  APos: TdxInputPosition;
begin
  ACount := Length(ALines);
  AMaxIndex := ACount - 1;
  if AMaxIndex < 0 then
    Exit;

  APos := TdxInputPosition.Create(ATable);
  try
    APos.CharacterFormatting.CopyFrom(ATable.Runs.First.CharacterProperties.Info);
    APos.CharacterStyleIndex := ATable.Runs.First.CharacterStyleIndex;

    for I := 0 to AMaxIndex - 1 do
    begin
      InsertTextLine(ATable, APos, ALines[I]);
      ATable.InsertParagraphCoreNoInheritParagraphRunStyle(APos);
    end;

    InsertTextLine(ATable, APos, ALines[AMaxIndex]);
  finally
    APos.Free;
  end;
end;

class procedure TdxTextManipulatorHelper.InsertTextLine(ATable: TdxPieceTable; APos:
  TdxInputPosition; const AText: string);
begin
  if AText <> '' then
    ATable.InsertTextCoreNoResetMergeNoApplyFormatting(APos, AText, False);
end;

end.
