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

unit dxRichEdit.Api.NativeDocumentBase;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Classes,
  dxRichEdit.InnerControl,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Intervals,
  dxRichEdit.DocumentModel.PieceTable.InternalApi;

type
  TdxNativeSubDocumentBase = class(TInterfacedObject)
  strict private
    FPieceTable: TdxPieceTable;
    FServer: TdxInnerRichEditDocumentServer;
    FIsValid: Boolean;
    function GetInternalAPI: TdxInternalAPI;
    function GetDocumentModel: TdxDocumentModel;
  strict protected
    procedure CreateApiObjects; virtual;
    procedure DestroyApiObjects; virtual;
    procedure DoInitialize; virtual;
    procedure DoFinalize; virtual;
    procedure DoSubscribeInternalAPIEvents; virtual;
    procedure DoUnsubscribeInternalAPIEvents; virtual;
  public
    constructor Create(APieceTable: TdxPieceTable; AServer: TdxInnerRichEditDocumentServer); virtual;
    destructor Destroy; override;

    procedure Initialize;
    procedure Finalize;
    procedure SubscribeInternalAPIEvents;
    procedure UnsubscribeInternalAPIEvents;

    procedure RegisterAnchor(APos: TdxDocumentModelPositionAnchor);
    procedure UnregisterAnchor(APos: TdxDocumentModelPositionAnchor);

    procedure UnitsChanged; virtual;

    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property DocumentServer: TdxInnerRichEditDocumentServer read FServer;
    property InternalAPI: TdxInternalAPI read GetInternalAPI;
    property PieceTable: TdxPieceTable read FPieceTable;
    property Server: TdxInnerRichEditDocumentServer read FServer;
  end;

implementation

{ TdxNativeSubDocumentBase }

constructor TdxNativeSubDocumentBase.Create(APieceTable: TdxPieceTable;
  AServer: TdxInnerRichEditDocumentServer);
begin
  Assert(APieceTable <> nil);
  Assert(AServer <> nil);
  inherited Create;
  FPieceTable := APieceTable;
  FServer := AServer;
  CreateApiObjects;
  Initialize;
end;

destructor TdxNativeSubDocumentBase.Destroy;
begin
  Finalize;
  DestroyApiObjects;
  inherited Destroy;
end;

procedure TdxNativeSubDocumentBase.CreateApiObjects;
begin
end;

procedure TdxNativeSubDocumentBase.DestroyApiObjects;
begin
end;

procedure TdxNativeSubDocumentBase.Initialize;
begin
  FIsValid := True;
  DoInitialize;
  SubscribeInternalAPIEvents;
end;

procedure TdxNativeSubDocumentBase.Finalize;
begin
  UnsubscribeInternalAPIEvents;
  DoFinalize;
  FIsValid := False;
end;

procedure TdxNativeSubDocumentBase.SubscribeInternalAPIEvents;
begin
  if FIsValid then
    DoSubscribeInternalAPIEvents;
end;

procedure TdxNativeSubDocumentBase.UnsubscribeInternalAPIEvents;
begin
  if FIsValid then
    DoUnsubscribeInternalAPIEvents;
end;

procedure TdxNativeSubDocumentBase.RegisterAnchor(APos: TdxDocumentModelPositionAnchor);
begin
  if not DocumentModel.IsDestroying then
    InternalApi.RegisterAnchor(APos);
end;

procedure TdxNativeSubDocumentBase.UnregisterAnchor(APos: TdxDocumentModelPositionAnchor);
begin
  if not DocumentModel.IsDestroying then
    InternalApi.UnregisterAnchor(APos);
end;

procedure TdxNativeSubDocumentBase.DoInitialize;
begin
// do nothing
end;

procedure TdxNativeSubDocumentBase.DoFinalize;
begin
// do nothing
end;

procedure TdxNativeSubDocumentBase.DoSubscribeInternalAPIEvents;
begin
// do nothing
end;

procedure TdxNativeSubDocumentBase.DoUnsubscribeInternalAPIEvents;
begin
// do nothing
end;

procedure TdxNativeSubDocumentBase.UnitsChanged;
begin
// do nothing
end;

function TdxNativeSubDocumentBase.GetDocumentModel: TdxDocumentModel;
begin
  Result := PieceTable.DocumentModel;
end;

function TdxNativeSubDocumentBase.GetInternalAPI: TdxInternalAPI;
begin
  Result := TdxInternalAPI(DocumentModel.InternalAPI);
end;

end.
