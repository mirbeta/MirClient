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

unit dxRichEdit.Import.FloatingObject;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  dxCoreClasses,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.DocumentModel.History.IndexChangedHistoryItem,
  dxRichEdit.DocumentModel.ShapeFormatting,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentModel.FloatingObjectFormatting,
  dxRichEdit.DocumentModel.Simple;

type
  TdxShapeType = (
    None = -1,
    PictureFrame = 75,
    TextBox = 202
  );

  { TdxFloatingObjectImportInfo }

  TdxFloatingObjectImportInfo = class(TcxIUnknownObject,
    IdxFloatingObjectPropertiesContainer,
    IdxShapeContainer,
    IdxTextBoxPropertiesContainer)
  strict private
    FFloatingObjectProperties: TdxFloatingObjectProperties;
    FShape: TdxShape;
    FTextBoxProperties: TdxTextBoxProperties;
    FPieceTable: TdxPieceTable;
    FTextBoxContent: TdxTextBoxContentType;
    FWidth: Integer;
    FHeight: Integer;
    FImage: TdxOfficeImageReference;
    FName: string;
    FIsFloatingObject: Boolean;
    FShapeType: TdxShapeType;
    FShouldIgnore: Boolean;
    function GetIsFloatingObject: Boolean;
  protected
    //IdxFloatingObjectPropertiesContainer
    function CreateFloatingObjectPropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
    function GetPieceTable: TdxCustomPieceTable;
    procedure OnFloatingObjectChanged;
    //IdxShapeContainer
    function CreateShapeChangedHistoryItem: TdxIndexChangedHistoryItemCore;
    procedure OnShapeChanged;
    //IdxTextBoxPropertiesContainer
    function CreateTextBoxChangedHistoryItem: TdxIndexChangedHistoryItemCore;
    procedure OnTextBoxChanged;

    property PieceTable: TdxPieceTable read FPieceTable;
  public
    constructor Create(APieceTable: TdxPieceTable);
    destructor Destroy; override;
    function InsertFloatingObject(APos: TdxInputPosition): Boolean;
    function InsertFloatingObjectCore(APos: TdxInputPosition): Boolean;

    property FloatingObjectProperties: TdxFloatingObjectProperties read FFloatingObjectProperties;
    property TextBoxContent: TdxTextBoxContentType read FTextBoxContent write FTextBoxContent;
    property Shape: TdxShape read FShape;
    property TextBoxProperties: TdxTextBoxProperties read FTextBoxProperties;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property Image: TdxOfficeImageReference read FImage write FImage;
    property IsFloatingObject: Boolean read GetIsFloatingObject write FIsFloatingObject;
    property ShapeType: TdxShapeType read FShapeType write FShapeType;
    property Name: string read FName write FName;
    property ShouldIgnore: Boolean read FShouldIgnore write FShouldIgnore;
  end;

implementation

uses
  SysUtils, dxCore;

{ TdxFloatingObjectImportInfo }

constructor TdxFloatingObjectImportInfo.Create(APieceTable: TdxPieceTable);
begin
  inherited Create;
  FWidth := MinInt;
  FHeight := MinInt;
  FName := '';
  FPieceTable := APieceTable;
  FFloatingObjectProperties := TdxFloatingObjectProperties.Create(Self);
  FShape := TdxShape.Create(Self);
  FTextBoxProperties := TdxTextBoxProperties.Create(Self);
  FFloatingObjectProperties.BeginUpdate;
  FShape.BeginUpdate;
  FTextBoxProperties.BeginUpdate;
end;

destructor TdxFloatingObjectImportInfo.Destroy;
begin
  FreeAndNil(FFloatingObjectProperties);
  FreeAndNil(FShape);
  FreeAndNil(FTextBoxProperties);
  inherited Destroy;
end;

function TdxFloatingObjectImportInfo.GetIsFloatingObject: Boolean;
begin
  Result := FIsFloatingObject and PieceTable.DocumentModel.DocumentCapabilities.FloatingObjectsAllowed;
end;

function TdxFloatingObjectImportInfo.InsertFloatingObject(APos: TdxInputPosition): Boolean;
begin
  Result := InsertFloatingObjectCore(APos);

  FTextBoxProperties.CancelUpdate;
  FShape.CancelUpdate;
  FFloatingObjectProperties.CancelUpdate;
end;

function TdxFloatingObjectImportInfo.InsertFloatingObjectCore(APos: TdxInputPosition): Boolean;
var
  ARun: TdxFloatingObjectAnchorRun;
  AContent: TdxTextBoxFloatingObjectContent;
begin
  Result := False;
  if not IsFloatingObject then
    Exit;

  if ShapeType = TdxShapeType.TextBox then
  begin
    if TextBoxContent = nil then
      Exit;

    ARun := PieceTable.InsertFloatingObjectAnchorCore(APos);
    AContent := TdxTextBoxFloatingObjectContent.Create(ARun, TextBoxContent);
    AContent.TextBoxProperties.CopyFrom(TextBoxProperties.Info);
    ARun.SetContent(AContent);
  end
  else
  begin
    if Image = nil then
      Exit;

    ARun := PieceTable.InsertFloatingObjectAnchorCore(APos);
    ARun.SetContent(TdxPictureFloatingObjectContent.Create(ARun, Image));
  end;
  ARun.FloatingObjectProperties.CopyFrom(FloatingObjectProperties.Info);
  ARun.Shape.CopyFrom(Shape.Info);
  ARun.Name := Name;
  Result := True;
end;

function TdxFloatingObjectImportInfo.GetPieceTable: TdxCustomPieceTable;
begin
  Result := FPieceTable;
end;

function TdxFloatingObjectImportInfo.CreateFloatingObjectPropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
begin
  Result := TdxIndexChangedHistoryItem.Create(PieceTable, FloatingObjectProperties);
end;

procedure TdxFloatingObjectImportInfo.OnFloatingObjectChanged;
begin
//do nothing
end;

procedure TdxFloatingObjectImportInfo.OnShapeChanged;
begin
//do nothing
end;

function TdxFloatingObjectImportInfo.CreateShapeChangedHistoryItem: TdxIndexChangedHistoryItemCore;
begin
  Result := TdxIndexChangedHistoryItem.Create(PieceTable, Shape);
end;

function TdxFloatingObjectImportInfo.CreateTextBoxChangedHistoryItem: TdxIndexChangedHistoryItemCore;
begin
  Result := TdxIndexChangedHistoryItem.Create(PieceTable, TextBoxProperties);
end;

procedure TdxFloatingObjectImportInfo.OnTextBoxChanged;
begin
//do nothing
end;

end.
