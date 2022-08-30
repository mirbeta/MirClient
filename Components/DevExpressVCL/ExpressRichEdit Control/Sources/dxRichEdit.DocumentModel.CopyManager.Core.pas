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

unit dxRichEdit.DocumentModel.CopyManager.Core;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Generics.Defaults, Generics.Collections,

  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Paragraphs;

type
  { TdxRunCharacterFormatting }

  TdxRunCharacterFormatting = record
  private
    FCharacterStyleIndex: Integer;
    FCharacterPropertiesIndex: Integer;
  public
    constructor Create(ACharacterStyleIndex, ACharacterPropertiesIndex: Integer);

    property CharacterStyleIndex: Integer read FCharacterStyleIndex;
    property CharacterPropertiesIndex: Integer read FCharacterPropertiesIndex;
  end;

  { TdxRunParagraphFormattingKey }

  TdxRunParagraphFormattingKey = record
  strict private
    FParagraphStyleIndex: Integer;
    FParagraphPropertiesIndex: Integer;
    FNumberingListIndex: TdxNumberingListIndex;
    FNumberingListLevel: Integer;
  public
    constructor Create(AParagraphStyleIndex: Integer; AParagraphPropertiesIndex: Integer; ANumberingListIndex: TdxNumberingListIndex; ANumberingListLevel: Integer);

    property ParagraphStyleIndex: Integer read FParagraphStyleIndex;
    property ParagraphPropertiesIndex: Integer read FParagraphPropertiesIndex;
    property NumberingListIndex: TdxNumberingListIndex read FNumberingListIndex;
    property NumberingListLevel: Integer read FNumberingListLevel;
  end;

  { TdxRunParagraphFormatting }

  TdxRunParagraphFormatting = record
  private
    FParagraphStyleIndex: Integer;
    FParagraphPropertiesIndex: Integer;
  public
    constructor Create(AParagraphStyleIndex, AParagraphPropertiesIndex: Integer);
    property ParagraphStyleIndex: Integer read FParagraphStyleIndex;
    property ParagraphPropertiesIndex: Integer read FParagraphPropertiesIndex;
  end;

  { TdxCustomDocumentModelCopyManager }

  TdxCustomDocumentModelCopyManager = class
  strict private
    FSourcePieceTable: TdxCustomPieceTable;
    FTargetPieceTable: TdxCustomPieceTable;
    FParagraphNumerationCopyOptions: TdxParagraphNumerationCopyOptions;
    FFormattingCopyOptions: TdxFormattingCopyOptions;
    FParagraphWasInsertedBeforeTable: Boolean;
    FIsInserted: Boolean;
    FMapSourceToTargetCharacterFormatting: TDictionary<TdxRunCharacterFormatting, TdxRunCharacterFormatting>;
    FMapSourceToTargetParagraphFormatting: TDictionary<TdxRunParagraphFormattingKey, TdxRunParagraphFormatting>;
  strict private
    function GetSourceModel: TdxCustomDocumentModel;
    function GetTargetModel: TdxCustomDocumentModel;
  strict protected
    FTargetPosition: TdxDocumentModelPosition;
    property MapSourceToTargetCharacterFormatting: TDictionary<TdxRunCharacterFormatting, TdxRunCharacterFormatting> read FMapSourceToTargetCharacterFormatting;
    property MapSourceToTargetParagraphFormatting: TDictionary<TdxRunParagraphFormattingKey, TdxRunParagraphFormatting> read FMapSourceToTargetParagraphFormatting;
  public
    constructor Create(ASourcePieceTable, ATargetPieceTable: TdxCustomPieceTable;
      AParagraphNumerationCopyOptions: TdxParagraphNumerationCopyOptions;
      AFormattingCopyOptions: TdxFormattingCopyOptions = TdxFormattingCopyOptions.UseDestinationStyles); virtual;
    destructor Destroy; override;

    procedure CopyAdditionalInfo(ACopyBetweenInternalModels: Boolean); virtual;

    property IsInserted: Boolean read FIsInserted write FIsInserted;
    property TargetPosition: TdxDocumentModelPosition read FTargetPosition;
    property SourcePieceTable: TdxCustomPieceTable read FSourcePieceTable;
    property SourceModel: TdxCustomDocumentModel read GetSourceModel;
    property TargetPieceTable: TdxCustomPieceTable read FTargetPieceTable;
    property TargetModel: TdxCustomDocumentModel read GetTargetModel;
    property ParagraphNumerationCopyOptions: TdxParagraphNumerationCopyOptions read FParagraphNumerationCopyOptions;
    property FormattingCopyOptions: TdxFormattingCopyOptions read FFormattingCopyOptions;
    property ParagraphWasInsertedBeforeTable: Boolean read FParagraphWasInsertedBeforeTable write FParagraphWasInsertedBeforeTable;
  end;

  { TdxCustomDeleteContentOperation }

  TdxCustomDeleteContentOperation = class abstract
  strict private
    FAllowPartiallyDeletingField: Boolean;
    FBackspacePressed: Boolean;
    FForceRemoveInnerFields: Boolean;
    FLeaveFieldIfResultIsRemoved: Boolean;
    FPieceTable: TdxCustomPieceTable;
    function GetDocumentModel: TdxCustomDocumentModel;
  public
    constructor Create(APieceTable: TdxCustomPieceTable);

    function Execute(AStartLogPosition: TdxDocumentLogPosition; ALength: Integer;
      ADocumentLastParagraphSelected: Boolean): Boolean; virtual; abstract;

    property AllowPartiallyDeletingField: Boolean read FAllowPartiallyDeletingField write FAllowPartiallyDeletingField;
    property BackspacePressed: Boolean read FBackspacePressed write FBackspacePressed;
    property DocumentModel: TdxCustomDocumentModel read GetDocumentModel;
    property ForceRemoveInnerFields: Boolean read FForceRemoveInnerFields write FForceRemoveInnerFields;
    property LeaveFieldIfResultIsRemoved: Boolean read FLeaveFieldIfResultIsRemoved write FLeaveFieldIfResultIsRemoved;
    property PieceTable: TdxCustomPieceTable read FPieceTable;
  end;

implementation

{ TdxRunCharacterFormatting }

constructor TdxRunCharacterFormatting.Create(ACharacterStyleIndex, ACharacterPropertiesIndex: Integer);
begin
  FCharacterStyleIndex := ACharacterStyleIndex;
  FCharacterPropertiesIndex := ACharacterPropertiesIndex;
end;

{ TdxRunParagraphFormattingKey }

constructor TdxRunParagraphFormattingKey.Create(AParagraphStyleIndex: Integer; AParagraphPropertiesIndex: Integer; ANumberingListIndex: TdxNumberingListIndex; ANumberingListLevel: Integer);
begin
  FParagraphStyleIndex := AParagraphStyleIndex;
  FParagraphPropertiesIndex := AParagraphPropertiesIndex;
  FNumberingListIndex := ANumberingListIndex;
  FNumberingListLevel := ANumberingListLevel;
end;

{ TdxRunParagraphFormatting }

constructor TdxRunParagraphFormatting.Create(
  AParagraphStyleIndex, AParagraphPropertiesIndex: Integer);
begin
  FParagraphStyleIndex := AParagraphStyleIndex;
  FParagraphPropertiesIndex := AParagraphPropertiesIndex;
end;

{ TdxCustomDocumentModelCopyManager }

constructor TdxCustomDocumentModelCopyManager.Create(ASourcePieceTable, ATargetPieceTable: TdxCustomPieceTable;
  AParagraphNumerationCopyOptions: TdxParagraphNumerationCopyOptions;
  AFormattingCopyOptions: TdxFormattingCopyOptions = TdxFormattingCopyOptions.UseDestinationStyles);
begin
  inherited Create;
  FSourcePieceTable := ASourcePieceTable;
  FTargetPieceTable := ATargetPieceTable;
  FTargetPosition := TdxDocumentModelPosition.Create(ATargetPieceTable);
  FParagraphNumerationCopyOptions := AParagraphNumerationCopyOptions;
  FFormattingCopyOptions := AFormattingCopyOptions;
  FMapSourceToTargetCharacterFormatting := TDictionary<TdxRunCharacterFormatting, TdxRunCharacterFormatting>.Create;
  FMapSourceToTargetParagraphFormatting := TDictionary<TdxRunParagraphFormattingKey, TdxRunParagraphFormatting>.Create;
end;

destructor TdxCustomDocumentModelCopyManager.Destroy;
begin
  FreeAndNil(FMapSourceToTargetParagraphFormatting);
  FreeAndNil(FMapSourceToTargetCharacterFormatting);
  inherited Destroy;
end;

procedure TdxCustomDocumentModelCopyManager.CopyAdditionalInfo(ACopyBetweenInternalModels: Boolean);
begin
end;

function TdxCustomDocumentModelCopyManager.GetSourceModel: TdxCustomDocumentModel;
begin
  Result := SourcePieceTable.DocumentModel;
end;

function TdxCustomDocumentModelCopyManager.GetTargetModel: TdxCustomDocumentModel;
begin
  Result := TargetPieceTable.DocumentModel;
end;

{ TdxCustomDeleteContentOperation }

constructor TdxCustomDeleteContentOperation.Create(
  APieceTable: TdxCustomPieceTable);
begin
  inherited Create;
  FPieceTable := APieceTable;
end;

function TdxCustomDeleteContentOperation.GetDocumentModel: TdxCustomDocumentModel;
begin
  Result := PieceTable.DocumentModel;
end;

end.
