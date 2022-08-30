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
unit dxRichEdit.Export.Doc.DocSectionPropertiesActions;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses,
  dxCoreGraphics, dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.PieceTable;

type

  { TdxDocSectionPropertiesActions }

  TdxDocSectionPropertiesActions = class
  strict private
    FWriter: TBinaryWriter;
    FSection: TdxSection;
  public
    constructor Create(AOutput: TdxMemoryStream; ASection: TdxSection);
    destructor Destroy; override;
    procedure CreateSectionPropertiesModifiers;
    function GetColumnsProperties: TBytes;
    function GetMarginsProperties: TBytes;
    function GetPageProperties: TBytes;
    function GetGeneralSettings: TBytes;
    function GetPageNumbering: TBytes;
    function GetLineNumbering: TBytes;
    function GetFootNote: TBytes;
    function GetEndNote: TBytes;
  end;

implementation

uses
  Math, Contnrs,
  dxRichEdit.Import.Doc.DocCommand,
  dxRichEdit.Export.Doc.DocColumnsActions,
  dxRichEdit.Export.Doc.DocMarginsActions,
  dxRichEdit.Export.Doc.DocPageActions,
  dxRichEdit.Export.Doc.DocGeneralSettingsActions,
  dxRichEdit.Export.Doc.DocPageNumberingActions,
  dxRichEdit.Export.Doc.DocLineNumberingActions,
  dxRichEdit.Export.Doc.DocFootNoteActions;

{ TdxDocSectionPropertiesActions }

constructor TdxDocSectionPropertiesActions.Create(AOutput: TdxMemoryStream; ASection: TdxSection);
begin
  FWriter := TBinaryWriter.Create(AOutput);
  FSection := ASection;
end;

destructor TdxDocSectionPropertiesActions.Destroy;
begin
  FWriter.Free;
  inherited Destroy;
end;

procedure TdxDocSectionPropertiesActions.CreateSectionPropertiesModifiers;
begin
  FWriter.Write(GetColumnsProperties);
  FWriter.Write(GetMarginsProperties);
  FWriter.Write(GetPageProperties);
  FWriter.Write(GetGeneralSettings);
  FWriter.Write(GetPageNumbering);
  FWriter.Write(GetLineNumbering);
  FWriter.Write(GetFootNote);
  FWriter.Write(GetEndNote);
end;

function TdxDocSectionPropertiesActions.GetColumnsProperties: TBytes;
var
  AColumnsStream: TdxMemoryStream;
  AActions: TdxDocColumnsActions;
begin
  AColumnsStream := TdxMemoryStream.Create;
  try
    AActions := TdxDocColumnsActions.Create(AColumnsStream, FSection.Columns);
    try
      AActions.ColumnCountAction;
      AActions.ColumnsAction;
      AActions.DrawVerticalSeparatorAction;
      AActions.EqualWidthColumnsAction;
      AActions.SpaceAction;
      Result := AColumnsStream.ToArray;
    finally
      AActions.Free;
    end;
  finally
    AColumnsStream.Free;
  end;
end;

function TdxDocSectionPropertiesActions.GetMarginsProperties: TBytes;
var
  AMarginsStream: TdxMemoryStream;
  AActions: TdxDocMarginsActions;
begin
  AMarginsStream := TdxMemoryStream.Create;
  try
    AActions := TdxDocMarginsActions.Create(AMarginsStream, FSection.Margins);
    try
      AActions.LeftAction;
      AActions.RightAction;
      AActions.TopAction;
      AActions.BottomAction;
      AActions.GutterAction;
      AActions.GutterAlignmentAction;
      AActions.HeaderOffsetAction;
      AActions.FooterOffsetAction;
      Result := AMarginsStream.ToArray;
    finally
      AActions.Free;
    end;
  finally
    AMarginsStream.Free;
  end;
end;

function TdxDocSectionPropertiesActions.GetPageProperties: TBytes;
var
  APageStream: TdxMemoryStream;
  AActions: TdxDocPageActions;
begin
  APageStream := TdxMemoryStream.Create;
  try
    AActions := TdxDocPageActions.Create(APageStream, FSection.Page);
    try
      AActions.HeightAction;
      AActions.LandscapeAction;
      AActions.PaperKindAction;
      AActions.WidthAction;
      Result := APageStream.ToArray;
    finally
      AActions.Free;
    end;
  finally
    APageStream.Free;
  end;
end;

function TdxDocSectionPropertiesActions.GetGeneralSettings: TBytes;
var
  AGeneralSettingsStream: TdxMemoryStream;
  AActions: TdxDocGeneralSettingsActions;
begin
  AGeneralSettingsStream := TdxMemoryStream.Create;
  try
    AActions := TdxDocGeneralSettingsActions.Create(AGeneralSettingsStream, FSection.GeneralSettings);
    try
      AActions.DifferentFirstPageAction;
      AActions.FirstPagePaperSourceAction;
      AActions.OnlyAllowEditingOfFormFieldsAction;
      AActions.OtherPagePaperSourceAction;
      AActions.StartTypeAction;
      AActions.TextDirectionAction;
      AActions.VerticalTextAlignmentAction;
      Result := AGeneralSettingsStream.ToArray;
    finally
      AActions.Free;
    end;
  finally
    AGeneralSettingsStream.Free;
  end;
end;

function TdxDocSectionPropertiesActions.GetPageNumbering: TBytes;
var
  APageNumberingStream: TdxMemoryStream;
  AActions: TdxDocPageNumberingActions;
begin
  APageNumberingStream := TdxMemoryStream.Create;
  try
    AActions := TdxDocPageNumberingActions.Create(APageNumberingStream, FSection.PageNumbering);
    try
      AActions.ChapterHeaderStyleAction;
      AActions.ChapterSeparatorAction;
      AActions.NumberingFormatAction;
      AActions.FirstPageNumberAction;
      Result := APageNumberingStream.ToArray;
    finally
      AActions.Free;
    end;
  finally
    APageNumberingStream.Free;
  end;
end;

function TdxDocSectionPropertiesActions.GetLineNumbering: TBytes;
var
  ALineNumberingStream: TdxMemoryStream;
  AActions: TdxDocLineNumberingActions;
begin
  ALineNumberingStream := TdxMemoryStream.Create;
  try
    AActions := TdxDocLineNumberingActions.Create(ALineNumberingStream, FSection.LineNumbering);
    try
      AActions.DistanceAction;
      AActions.NumberingRestartTypeAction;
      AActions.StartingLineNumberAction;
      AActions.StepAction;
      Result := ALineNumberingStream.ToArray;
    finally
      AActions.Free;
    end;
  finally
    ALineNumberingStream.Free;
  end;
end;

function TdxDocSectionPropertiesActions.GetFootNote: TBytes;
var
  AFootNoteStream: TdxMemoryStream;
  AActions: TdxDocFootNoteActions;
begin
  AFootNoteStream := TdxMemoryStream.Create;
  try
    AActions := TdxDocFootNoteActions.Create(AFootNoteStream, FSection.FootNote, True);
    try
      AActions.NumberingFormatAction;
      AActions.NumberingRestartTypeAction;
      AActions.PositionAction;
      AActions.StartingNumberAction;
      Result := AFootNoteStream.ToArray;
    finally
      AActions.Free;
    end;
  finally
    AFootNoteStream.Free;
  end;
end;

function TdxDocSectionPropertiesActions.GetEndNote: TBytes;
var
  AEndNoteStream: TdxMemoryStream;
  AActions: TdxDocFootNoteActions;
begin
  AEndNoteStream := TdxMemoryStream.Create;
  try
    AActions := TdxDocFootNoteActions.Create(AEndNoteStream, FSection.EndNote, False);
    try
      AActions.NumberingFormatAction;
      AActions.NumberingRestartTypeAction;
      AActions.PositionAction;
      AActions.StartingNumberAction;
      Result := AEndNoteStream.ToArray;
    finally
      AActions.Free;
    end;
  finally
    AEndNoteStream.Free;
  end;
end;

end.
