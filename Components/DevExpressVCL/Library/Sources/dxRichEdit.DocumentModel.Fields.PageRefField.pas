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

unit dxRichEdit.DocumentModel.Fields.PageRefField;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Generics.Defaults, Generics.Collections,
  dxCoreClasses,
  dxRichEdit.Options.Simple,
  dxRichEdit.Options,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.MailMerge,
  dxRichEdit.DocumentModel.Bookmarks,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields;

type
  { TdxPageRefField }

  TdxPageRefField = class(TdxCalculatedFieldBase)
  public const
    FieldType = 'PAGEREF';
  strict private
    class var
      FSwitchesWithArgument: TdxStringBooleanDictionary;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FCreateHyperlink: Boolean;
    FRelativePosition: Boolean;
    FBookmarkName: string;
    FTargetPageNumber: Integer;
  protected
    function GetSwitchesWithArguments: TdxStringBooleanDictionary; override;
    function GetFieldTypeName: string; override;
    function GetCanPrepare: Boolean; override;
  public
    constructor Create; override;
    class function CreateField: TdxCalculatedFieldBase; override;
    procedure Initialize(APieceTable: TdxCustomPieceTable; AInstructions: TdxInstructionCollection); override;
    procedure BeforeCalculateFields(ASourcePieceTable: TdxCustomPieceTable; ADocumentField: TdxField); override;
    function FindBookmarkByName(ASourcePieceTable: TdxPieceTable; const ABookmarkName: string): TdxBookmark;
    function GetAllowedUpdateFieldTypes(const AOptions: TdxFieldUpdateOnLoadOptions): TdxUpdateFieldOperationTypes; override;
    function GetCalculatedValueCore(ASourcePieceTable: TdxCustomPieceTable; AMailMergeDataMode: TdxMailMergeDataMode; ADocumentField: TdxField): TdxCalculatedFieldValue; override;
    function GetHyperlinkText: string;
    function GetHyperlinkCode: string;

    property CreateHyperlink: Boolean read FCreateHyperlink;
    property RelativePosition: Boolean read FRelativePosition;
  end;

  { IdxBookmarkResolutionService }

  IdxBookmarkResolutionService = interface
  ['{D12E323E-F97D-4C77-9866-694E1871E50D}']
    function FindBookmarkByName(const AName: string): TdxBookmark;
  end;

  TdxBookmarkResolutionService = class(TInterfacedObject, IdxBookmarkResolutionService)
  strict private
    FPieceTable: TdxPieceTable;
  protected
    // IdxBookmarkResolutionService
    function FindBookmarkByName(const AName: string): TdxBookmark;
  public
    constructor Create(APieceTable: TdxPieceTable);
  end;

implementation

uses
  Rtti,
  dxRichEdit.InternalRichEditDocumentServer,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentLayout,
  dxRichEdit.DocumentLayout.Position;

{ TdxPageRefField }

class constructor TdxPageRefField.Initialize;
begin
  FSwitchesWithArgument := CreateSwitchesWithArgument;
end;

class destructor TdxPageRefField.Finalize;
begin
  FreeAndNil(FSwitchesWithArgument);
end;

constructor TdxPageRefField.Create;
begin
  inherited Create;
  FTargetPageNumber := -1;
end;

class function TdxPageRefField.CreateField: TdxCalculatedFieldBase;
begin
  Result := TdxPageRefField.Create;
end;

function TdxPageRefField.GetSwitchesWithArguments: TdxStringBooleanDictionary;
begin
  Result := FSwitchesWithArgument;
end;

function TdxPageRefField.GetFieldTypeName: string;
begin
  Result := FieldType;
end;

function TdxPageRefField.GetCanPrepare: Boolean;
begin
  Result := True;
end;

procedure TdxPageRefField.Initialize(APieceTable: TdxCustomPieceTable; AInstructions: TdxInstructionCollection);
begin
  inherited Initialize(APieceTable, AInstructions);
  FCreateHyperlink := AInstructions.GetBool('h');
  FRelativePosition := AInstructions.GetBool('p');
  FBookmarkName := AInstructions.GetArgumentAsString(0);
end;

function TdxPageRefField.GetAllowedUpdateFieldTypes(const AOptions: TdxFieldUpdateOnLoadOptions): TdxUpdateFieldOperationTypes;
begin
  Result := [TdxUpdateFieldOperationType.Normal];
end;

procedure TdxPageRefField.BeforeCalculateFields(ASourcePieceTable: TdxCustomPieceTable; ADocumentField: TdxField);
var
  ATable: TdxPieceTable absolute ASourcePieceTable;
  ADocumentLayoutService: IdxDocumentLayoutService;
  ADocumentLayout: TdxDocumentLayout;
  ABookmark: TdxBookmark;
  ALayoutPosition: TdxDocumentLayoutPosition;
begin
  FTargetPageNumber := -1;
  ADocumentLayoutService := ATable.DocumentModel.GetService<IdxDocumentLayoutService>;
  if ADocumentLayoutService = nil then
    Exit;
  ADocumentLayout := ADocumentLayoutService.CalculateDocumentLayout;
  ABookmark := FindBookmarkByName(ATable, FBookmarkName);
  if ABookmark = nil then
    Exit;
  ALayoutPosition := TdxDocumentLayoutPosition(ADocumentLayout.CreateLayoutPosition(ABookmark.PieceTable, ABookmark.NormalizedStart, -1));
  try
    ALayoutPosition.Update(ADocumentLayout.Pages, TdxDocumentLayoutDetailsLevel.Page);
    if not ALayoutPosition.IsValid(TdxDocumentLayoutDetailsLevel.Page) then
      Exit;
    FTargetPageNumber := ALayoutPosition.Page.PageOrdinal;
  finally
    ALayoutPosition.Free;
  end;
end;

function TdxPageRefField.FindBookmarkByName(ASourcePieceTable: TdxPieceTable; const ABookmarkName: string): TdxBookmark;
var
  AResolutionService: IdxBookmarkResolutionService;
begin
  AResolutionService := ASourcePieceTable.DocumentModel.GetService<IdxBookmarkResolutionService>;
  if AResolutionService = nil then
    Result := ASourcePieceTable.Bookmarks.FindByName(ABookmarkName)
  else
    Result := AResolutionService.FindBookmarkByName(ABookmarkName);
end;

function TdxPageRefField.GetCalculatedValueCore(ASourcePieceTable: TdxCustomPieceTable;
  AMailMergeDataMode: TdxMailMergeDataMode; ADocumentField: TdxField): TdxCalculatedFieldValue;
var
  ACode, AText: string;
  ATargetModel: TdxDocumentModel;
  ATargetPieceTable: TdxPieceTable;
  AField: TdxField;
begin
  if FTargetPageNumber < 0 then
    Exit(TdxCalculatedFieldValue.Create(''));
  if FCreateHyperlink then
  begin
    ACode := GetHyperlinkCode;
    AText := GetHyperlinkText;
    ATargetModel := TdxPieceTable(ASourcePieceTable).DocumentModel.GetFieldResultModel;

    ATargetPieceTable := ATargetModel.MainPieceTable;
    ATargetPieceTable.InsertText(0, ACode);
    AField := ATargetPieceTable.CreateField(0, Length(ACode));
    ATargetPieceTable.InsertText(Length(ACode) + 2, AText);
    ATargetPieceTable.FieldUpdater.UpdateField(AField, AMailMergeDataMode);
    Result := TdxCalculatedFieldValue.Create(TValue.From<TdxDocumentModel>(ATargetModel));
  end
  else
    Result := TdxCalculatedFieldValue.Create(FTargetPageNumber);
end;

function TdxPageRefField.GetHyperlinkText: string;
begin
  Result := Format('%d', [FTargetPageNumber]);
end;

function TdxPageRefField.GetHyperlinkCode: string;
begin
  Result := Format('HYPERLINK \l "%s"', [FBookmarkName]);
end;

{ TdxBookmarkResolutionService }

constructor TdxBookmarkResolutionService.Create(APieceTable: TdxPieceTable);
begin
  inherited Create;
  FPieceTable := APieceTable;
end;

function TdxBookmarkResolutionService.FindBookmarkByName(
  const AName: string): TdxBookmark;
begin
  Result := FPieceTable.Bookmarks.FindByName(AName);
end;

end.
