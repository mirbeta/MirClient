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

unit dxRichEdit.DocumentModel.Selections.Core;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Contnrs,
  dxCore, dxCoreClasses,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxGenerics,
  dxRichEdit.DocumentModel.Core;

type
  { TdxSelectionRange }

  TdxSelectionRange = class
  strict private
    FFrom: TdxDocumentLogPosition;
    FLength: Integer;
    function GetEnd: TdxDocumentLogPosition;
  public
    constructor Create(AFrom: TdxDocumentLogPosition; ALength: Integer);
    function CompareTo(AOther: TdxSelectionRange): Integer;

    property From: TdxDocumentLogPosition read FFrom write FFrom;
    property Length: Integer read FLength write FLength;
    property Start: TdxDocumentLogPosition read FFrom;
    property &End: TdxDocumentLogPosition read GetEnd;
  end;

  { TdxSelectionRangeCollection }

  TdxSelectionRangeCollection = class(TdxObjectList<TdxSelectionRange>)
  public
    constructor Create; overload;
    constructor Create(AFrom: TdxDocumentLogPosition; ALength: Integer); overload;

    function First: TdxSelectionRange;
    function Last: TdxSelectionRange;
  end;

  { TdxSelectionPersistentInfo }

  TdxSelectionPersistentInfo = class
  strict private
    FPieceTable: TdxCustomPieceTable;
    FEnd: TdxDocumentLogPosition;
    FStart: TdxDocumentLogPosition;
  public
    constructor Create(APieceTable: TdxCustomPieceTable);

    property PieceTable: TdxCustomPieceTable read FPieceTable;
    property &End: TdxDocumentLogPosition read FEnd write FEnd;
    property Start: TdxDocumentLogPosition read FStart write FStart;
  end;

  { TdxCustomSelection }

  TdxCustomSelection = class abstract(TcxIUnknownObject,
    IdxBatchUpdateHandler)
  strict private
    type
      TRangeComparer = class(TdxComparer<TdxSelectionRange>)
      public
        function Compare(const Left, Right: TdxSelectionRange): Integer; override;
      end;
    class var
      FRangeComparer: TRangeComparer;
    class constructor Initialize;
    class destructor Finalize;
  strict protected
    class property RangeComparer: TRangeComparer read FRangeComparer;
  strict private
    FActiveSelectionChanged: Boolean;
    FBatchUpdateHelper: TdxBatchUpdateHelper;
    FChangedEvent: TdxEventHandler;
    FPieceTable: TdxCustomPieceTable;
    function GetIsUpdateLocked: Boolean;
  strict protected
    function GetIsSelectionChanged: Boolean; virtual; abstract;
    procedure SetIsSelectionChanged(const AValue: Boolean); virtual; abstract;

    property OwnActiveSelectionChanged: Boolean read FActiveSelectionChanged write FActiveSelectionChanged;
  protected
  {$REGION 'IdxBatchUpdateHandler'}
    procedure OnFirstBeginUpdate;
    procedure OnBeginUpdate;
    procedure OnCancelUpdate;
    procedure OnEndUpdate;
    procedure OnLastEndUpdate; virtual; abstract;
    procedure OnLastCancelUpdate; virtual; abstract;
  {$ENDREGION}
    property ActiveSelectionChanged: Boolean read FActiveSelectionChanged;
  public
    constructor Create(APieceTable: TdxCustomPieceTable); virtual;
    destructor Destroy; override;

    procedure BeginUpdate; virtual;
    procedure EndUpdate;  virtual;

    property BatchUpdateHelper: TdxBatchUpdateHelper read FBatchUpdateHelper;
    property IsUpdateLocked: Boolean read GetIsUpdateLocked;
    property IsSelectionChanged: Boolean read GetIsSelectionChanged write SetIsSelectionChanged;
    property PieceTable: TdxCustomPieceTable read FPieceTable;
    property Changed: TdxEventHandler read FChangedEvent write FChangedEvent;
  end;

implementation

{ TdxSelectionRange }

constructor TdxSelectionRange.Create(AFrom: TdxDocumentLogPosition; ALength: Integer);
begin
  inherited Create;
  FFrom := AFrom;
  FLength := ALength;
end;

function TdxSelectionRange.CompareTo(AOther: TdxSelectionRange): Integer;
begin
  Result := From - AOther.From;
end;

function TdxSelectionRange.GetEnd: TdxDocumentLogPosition;
begin
  Result := From + Length;
end;

{ TdxSelectionRangeCollection }

constructor TdxSelectionRangeCollection.Create;
begin
  inherited Create;
end;

constructor TdxSelectionRangeCollection.Create(AFrom: TdxDocumentLogPosition; ALength: Integer);
begin
  inherited Create;
  Add(TdxSelectionRange.Create(AFrom, ALength));
end;

function TdxSelectionRangeCollection.First: TdxSelectionRange;
begin
  Result := nil;
  if Count > 0 then
    Result := Items[0];
end;

function TdxSelectionRangeCollection.Last: TdxSelectionRange;
begin
  Result := nil;
  if Count > 0 then
    Result := Items[Count - 1];
end;

{ TdxSelectionPersistentInfo }

constructor TdxSelectionPersistentInfo.Create(APieceTable: TdxCustomPieceTable);
begin
  inherited Create;
  FPieceTable := APieceTable;
end;

{ TdxCustomSelection.TRangeComparer }

function TdxCustomSelection.TRangeComparer.Compare(const Left, Right: TdxSelectionRange): Integer;
begin
  Result := Left.From - Right.From;
end;

{ TdxCustomSelection }

class constructor TdxCustomSelection.Initialize;
begin
  FRangeComparer := TRangeComparer.Create;
end;

class destructor TdxCustomSelection.Finalize;
begin
  FreeAndNil(FRangeComparer);
end;

constructor TdxCustomSelection.Create(APieceTable: TdxCustomPieceTable);
begin
  inherited Create;
  FPieceTable := APieceTable;
  FBatchUpdateHelper := TdxBatchUpdateHelper.Create(Self);
end;

destructor TdxCustomSelection.Destroy;
begin
  FreeAndNil(FBatchUpdateHelper);
  inherited Destroy;
end;

procedure TdxCustomSelection.BeginUpdate;
begin
  BatchUpdateHelper.BeginUpdate;
end;

procedure TdxCustomSelection.EndUpdate;
begin
  BatchUpdateHelper.EndUpdate;
end;

procedure TdxCustomSelection.OnBeginUpdate;
begin
end;

procedure TdxCustomSelection.OnCancelUpdate;
begin
end;

procedure TdxCustomSelection.OnEndUpdate;
begin
end;

procedure TdxCustomSelection.OnFirstBeginUpdate;
begin
  FActiveSelectionChanged := False;
end;

function TdxCustomSelection.GetIsUpdateLocked: Boolean;
begin
  Result := FBatchUpdateHelper.IsUpdateLocked;
end;

end.
