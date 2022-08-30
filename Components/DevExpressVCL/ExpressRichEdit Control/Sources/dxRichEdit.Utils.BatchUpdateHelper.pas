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

unit dxRichEdit.Utils.BatchUpdateHelper;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  dxCoreClasses, cxClasses;

type
  TdxBatchUpdateHelper = class;

  IdxBatchUpdateable = interface
  ['{EE87C565-1BFA-4979-8295-371F7E26B46B}']
    function GetIsUpdateLocked: Boolean;
    function GetBatchUpdateHelper: TdxBatchUpdateHelper;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure CancelUpdate;

    property IsUpdateLocked: Boolean read GetIsUpdateLocked;
    property BatchUpdateHelper: TdxBatchUpdateHelper read GetBatchUpdateHelper;
  end;

  IdxBatchUpdateHandler = interface
  ['{EC2404C7-CF0A-42BA-AF06-B2AC1F72098E}']
    procedure OnFirstBeginUpdate;
    procedure OnBeginUpdate;
    procedure OnEndUpdate;
    procedure OnLastEndUpdate;
    procedure OnCancelUpdate;
    procedure OnLastCancelUpdate;
  end;

  IdxBatchInit = interface
  ['{C8DCD361-A842-4555-A56B-BCA54604C533}']
    procedure BeginInit;
    procedure EndInit;
    procedure CancelInit;
  end;

  IdxBatchInitHandler = interface
  ['{18F85D91-1B68-4D66-A7B2-C7A4BCB535F2}']
    procedure OnBeginInit;
    procedure OnEndInit;
    procedure OnFirstBeginInit;
    procedure OnLastEndInit;
    procedure OnCancelInit;
    procedure OnLastCancelInit;
  end;

  { TdxBatchUpdateHelper }

  TdxBatchUpdateHelper = class
  public type
    TLifetimeStrategy = (Persistent, FreeAfterTransaction);
  strict private
    FBatchUpdateHandler: IdxBatchUpdateHandler;
    FLifetimeStrategy: TLifetimeStrategy;
    FSuspendUpdateCount: Integer;
    FOverlappedTransaction: Boolean;
    function GetIsUpdateLocked: Boolean;
  public
    constructor Create(const ABatchUpdateHandler: IdxBatchUpdateHandler; ALifetimeStrategy: TLifetimeStrategy = TLifetimeStrategy.Persistent);

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure CancelUpdate;

    property IsUpdateLocked: Boolean read GetIsUpdateLocked;
    property SuspendUpdateCount: Integer read FSuspendUpdateCount;
    property BatchUpdateHandler: IdxBatchUpdateHandler read FBatchUpdateHandler write FBatchUpdateHandler;
    property OverlappedTransaction: Boolean read FOverlappedTransaction;
  end;

  TdxBatchUpdateHelper<T> = class(TdxBatchUpdateHelper)
  private
    FDeferredNotifications: T;
    FFakeAssignDetected: Boolean;
    FSuppressDirectNotificationsCount: Integer;
    FSuppressIndexRecalculationOnEndInitCount: Integer;
    function GetIsDirectNotificationsEnabled: Boolean;
    function GetIsIndexRecalculationOnEndInitEnabled: Boolean;
  public
    destructor Destroy; override;
    procedure ResumeDirectNotifications;
    procedure SuppressDirectNotifications;
    procedure ResumeIndexRecalculationOnEndInit;
    procedure SuppressIndexRecalculationOnEndInit;

    property DeferredNotifications: T read FDeferredNotifications write FDeferredNotifications;
    property FakeAssignDetected: Boolean read FFakeAssignDetected write FFakeAssignDetected;
    property IsDirectNotificationsEnabled: Boolean read GetIsDirectNotificationsEnabled;
    property IsIndexRecalculationOnEndInitEnabled: Boolean read GetIsIndexRecalculationOnEndInitEnabled;
  end;

  TdxBatchUpdateHelper<TInfo, TOptions> = class(TdxBatchUpdateHelper)
  private
    FDeferredInfoNotifications: TInfo;
    FDeferredOptionsNotifications: TOptions;
  public
    destructor Destroy; override;
    property DeferredInfoNotifications: TInfo read FDeferredInfoNotifications write FDeferredInfoNotifications;
    property DeferredOptionsNotifications: TOptions read FDeferredOptionsNotifications write FDeferredOptionsNotifications;
  end;

  TdxBatchInitHelper<T> = class(TdxBatchUpdateHelper<T>)
  strict private type
    TInnerBatchUpdateHandler = class(TInterfacedObject, IdxBatchUpdateHandler)
    private
      FBatchInitHandler: IdxBatchInitHandler;
    public
      constructor Create(const ABatchInitHandler: IdxBatchInitHandler);

      procedure OnFirstBeginUpdate;
      procedure OnBeginUpdate;
      procedure OnEndUpdate;
      procedure OnLastEndUpdate;
      procedure OnCancelUpdate;
      procedure OnLastCancelUpdate;

      property BatchInitHandler: IdxBatchInitHandler read FBatchInitHandler;
    end;
  strict private
    FBatchInitHandler: TInnerBatchUpdateHandler;
    function GetBatchInitHandler: IdxBatchInitHandler;
  public
    constructor Create(const AHandler: IdxBatchInitHandler);

    property BatchInitHandler: IdxBatchInitHandler read GetBatchInitHandler;
  end;

implementation

uses
  SysUtils;

{ TdxBatchUpdateHelper }

constructor TdxBatchUpdateHelper.Create(const ABatchUpdateHandler: IdxBatchUpdateHandler; ALifetimeStrategy: TLifetimeStrategy = TLifetimeStrategy.Persistent);
begin
  inherited Create;
  FBatchUpdateHandler := ABatchUpdateHandler;
  FLifetimeStrategy := ALifetimeStrategy;
end;

procedure TdxBatchUpdateHelper.BeginUpdate;
begin
	if FOverlappedTransaction then
    Exit;
  if not IsUpdateLocked then
  begin
    FOverlappedTransaction := True;
    try
      FBatchUpdateHandler.OnFirstBeginUpdate;
    finally
      FOverlappedTransaction := False;
    end;
  end;
  FBatchUpdateHandler.OnBeginUpdate;
  Inc(FSuspendUpdateCount);
end;

procedure TdxBatchUpdateHelper.CancelUpdate;
var
  ALifetimeStrategy: TLifetimeStrategy;
begin
  if FOverlappedTransaction then
    Exit;
  if IsUpdateLocked then
  begin
    FBatchUpdateHandler.OnCancelUpdate;
    Dec(FSuspendUpdateCount);
    if not IsUpdateLocked then
    begin
      ALifetimeStrategy := FLifetimeStrategy;
      FOverlappedTransaction := True;
      FBatchUpdateHandler.OnLastCancelUpdate;
      if ALifetimeStrategy = TLifetimeStrategy.Persistent then
        FOverlappedTransaction := False;
    end;
  end;
end;

procedure TdxBatchUpdateHelper.EndUpdate;
var
  ALifetimeStrategy: TLifetimeStrategy;
begin
  if FOverlappedTransaction then
    Exit;
  if IsUpdateLocked then
  begin
    FBatchUpdateHandler.OnEndUpdate;
    Dec(FSuspendUpdateCount);
    if not IsUpdateLocked then
    begin
      ALifetimeStrategy := FLifetimeStrategy;
      FOverlappedTransaction := True;
      FBatchUpdateHandler.OnLastEndUpdate;
      if ALifetimeStrategy = TLifetimeStrategy.Persistent then
        FOverlappedTransaction := False;
    end;
  end;
end;

function TdxBatchUpdateHelper.GetIsUpdateLocked: Boolean;
begin
  Result := FSuspendUpdateCount > 0;
end;

{ TdxBatchUpdateHelper<T> }

procedure TdxBatchUpdateHelper<T>.ResumeDirectNotifications;
begin
  Dec(FSuppressDirectNotificationsCount);
end;

procedure TdxBatchUpdateHelper<T>.SuppressDirectNotifications;
begin
  Inc(FSuppressDirectNotificationsCount);
end;

procedure TdxBatchUpdateHelper<T>.ResumeIndexRecalculationOnEndInit;
begin
  Dec(FSuppressIndexRecalculationOnEndInitCount);
end;

procedure TdxBatchUpdateHelper<T>.SuppressIndexRecalculationOnEndInit;
begin
  Inc(FSuppressIndexRecalculationOnEndInitCount);
end;

destructor TdxBatchUpdateHelper<T>.Destroy;
begin
  //FreeAndNil(FDeferredNotifications);
  inherited Destroy;
end;

function TdxBatchUpdateHelper<T>.GetIsDirectNotificationsEnabled: Boolean;
begin
  Result := FSuppressDirectNotificationsCount = 0;
end;

function TdxBatchUpdateHelper<T>.GetIsIndexRecalculationOnEndInitEnabled: Boolean;
begin
  Result := FSuppressIndexRecalculationOnEndInitCount = 0;
end;

{ TdxBatchUpdateHelper<TInfo, TOptions> }

destructor TdxBatchUpdateHelper<TInfo, TOptions>.Destroy;
begin
  //FreeAndNil(FDeferredInfoNotifications);
  inherited Destroy;
end;

{ TdxBatchInitHelper<T>.TInnerBatchUpdateHandler }

constructor TdxBatchInitHelper<T>.TInnerBatchUpdateHandler.Create(
  const ABatchInitHandler: IdxBatchInitHandler);
begin
  inherited Create;
  Assert(ABatchInitHandler <> nil);
  FBatchInitHandler := ABatchInitHandler;
end;

procedure TdxBatchInitHelper<T>.TInnerBatchUpdateHandler.OnBeginUpdate;
begin
  BatchInitHandler.OnBeginInit;
end;

procedure TdxBatchInitHelper<T>.TInnerBatchUpdateHandler.OnCancelUpdate;
begin
  BatchInitHandler.OnCancelInit;
end;

procedure TdxBatchInitHelper<T>.TInnerBatchUpdateHandler.OnEndUpdate;
begin
  BatchInitHandler.OnEndInit;
end;

procedure TdxBatchInitHelper<T>.TInnerBatchUpdateHandler.OnFirstBeginUpdate;
begin
  BatchInitHandler.OnFirstBeginInit;
end;

procedure TdxBatchInitHelper<T>.TInnerBatchUpdateHandler.OnLastCancelUpdate;
begin
  BatchInitHandler.OnLastCancelInit;
end;

procedure TdxBatchInitHelper<T>.TInnerBatchUpdateHandler.OnLastEndUpdate;
begin
  BatchInitHandler.OnLastEndInit;
end;

{ TdxBatchInitHelper<T> }

constructor TdxBatchInitHelper<T>.Create(const AHandler: IdxBatchInitHandler);
begin
  FBatchInitHandler := TInnerBatchUpdateHandler.Create(AHandler);
  inherited Create(FBatchInitHandler, TLifetimeStrategy.FreeAfterTransaction);
end;

function TdxBatchInitHelper<T>.GetBatchInitHandler: IdxBatchInitHandler;
begin
  Result := FBatchInitHandler.BatchInitHandler;
end;

end.
