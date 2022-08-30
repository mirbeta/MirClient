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

unit dxRichEdit.Utils.BackgroundThreadUIUpdater;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SyncObjs, Classes, Messages, Generics.Defaults, Generics.Collections, dxCoreClasses,
  dxRichEdit.Utils.Types;

type

  { TdxObjectProcedure }

  TdxOwnedProcedure = record
    Owner: TObject;
    ProcedureRef: TdxAction;
  end;

  { TdxBackgroundThreadUIUpdater }

  TdxBackgroundThreadUIUpdater = class abstract
  public
    procedure UpdateUI(ASender: TObject; const AMethod: TdxAction); overload; virtual; abstract;
    procedure UpdateUI(const AUpdate: TdxOwnedProcedure); overload; virtual; abstract;
  end;

  { TdxBeginInvokeBackgroundThreadUIUpdater }

  TdxBeginInvokeBackgroundThreadUIUpdater = class(TdxBackgroundThreadUIUpdater)
  public
    procedure UpdateUI(ASender: TObject; const AMethod: TdxAction); overload; override;
    procedure UpdateUI(const AUpdate: TdxOwnedProcedure); overload; override;
  end;

  { TdxDeferredBackgroundThreadUIUpdater }

  TdxDeferredBackgroundThreadUIUpdater = class(TdxBackgroundThreadUIUpdater)
  strict private
    FUpdates: TList<TdxOwnedProcedure>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure UpdateUI(ASender: TObject; const AMethod: TdxAction); override;

    property Updates: TList<TdxOwnedProcedure> read FUpdates;
  end;

implementation

uses
  SysUtils, dxThreading;

{ TdxBeginInvokeBackgroundThreadUIUpdater }

procedure TdxBeginInvokeBackgroundThreadUIUpdater.UpdateUI(ASender: TObject; const AMethod: TdxAction);
begin
  TdxUIThreadSyncService.EnqueueInvokeInUIThread(ASender, AMethod);
end;

procedure TdxBeginInvokeBackgroundThreadUIUpdater.UpdateUI(const AUpdate: TdxOwnedProcedure);
begin
  TdxUIThreadSyncService.EnqueueInvokeInUIThread(AUpdate.Owner, AUpdate.ProcedureRef);
end;

{ TdxDeferredBackgroundThreadUIUpdater }

constructor TdxDeferredBackgroundThreadUIUpdater.Create;
begin
  inherited Create;
  FUpdates := TList<TdxOwnedProcedure>.Create;
end;

destructor TdxDeferredBackgroundThreadUIUpdater.Destroy;
begin
  FreeAndNil(FUpdates);
  inherited Destroy;
end;

procedure TdxDeferredBackgroundThreadUIUpdater.UpdateUI(ASender: TObject; const AMethod: TdxAction);
var
  AUpdate: TdxOwnedProcedure;
  AIndex: Integer;
begin
  AUpdate.Owner := ASender;
  AUpdate.ProcedureRef := AMethod;
  AIndex := FUpdates.IndexOf(AUpdate);
  if AIndex >= 0 then
    FUpdates.Delete(AIndex);
  FUpdates.Add(AUpdate);
end;

end.
