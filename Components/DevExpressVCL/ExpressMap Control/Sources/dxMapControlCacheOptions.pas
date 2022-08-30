{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressMapControl                                        }
{                                                                    }
{           Copyright (c) 2013-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSMAPCONTROL AND ALL             }
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

unit dxMapControlCacheOptions;

interface

{$I cxVer.inc}

uses
  Classes, Math, dxCoreClasses;

type
  TdxMapControlCacheOptions = class(TcxOwnedPersistent)
  private
    FMemoryLimit: Integer;
    FDiskLimit: Integer;
    FDiskExpireTime: Int64;
    FDiskFolder: string;
    FOnChanged: TNotifyEvent;
    procedure DoChanged;
    procedure SetMemoryLimit(Value: Integer);
    procedure SetDiskLimit(Value: Integer);
    procedure SetDiskExpireTime(Value: Int64);
    procedure SetDiskFolder(Value: string);
  protected
    procedure DoAssign(Source: TPersistent); override;
  public
    constructor Create(AOwner: TPersistent); override;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  published
    property MemoryLimit: Integer read FMemoryLimit write SetMemoryLimit default 128;
    property DiskLimit: Integer read FDiskLimit write SetDiskLimit default -1;
    property DiskExpireTime: Int64 read FDiskExpireTime write SetDiskExpireTime default 0;
    property DiskFolder: string read FDiskFolder write SetDiskFolder;
  end;

implementation

{ TdxMapControlCacheOptions }

constructor TdxMapControlCacheOptions.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FMemoryLimit := 128;
  FDiskLimit := -1;
  FDiskFolder := '';
end;

procedure TdxMapControlCacheOptions.DoAssign(Source: TPersistent);
var
  ACacheOptions: TdxMapControlCacheOptions;
begin
  inherited;
  if Source is TdxMapControlCacheOptions then
  begin
    ACacheOptions := TdxMapControlCacheOptions(Source);
    MemoryLimit := ACacheOptions.MemoryLimit;
    DiskLimit := ACacheOptions.DiskLimit;
    DiskExpireTime := ACacheOptions.DiskExpireTime;
    DiskFolder := ACacheOptions.DiskFolder;
  end;
end;

procedure TdxMapControlCacheOptions.DoChanged;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TdxMapControlCacheOptions.SetMemoryLimit(Value: Integer);
begin
  Value := Max(1, Value);
  if (FMemoryLimit <> Value) then
  begin
    FMemoryLimit := Value;
    DoChanged;
  end;
end;

procedure TdxMapControlCacheOptions.SetDiskLimit(Value: Integer);
begin
  if (FDiskLimit <> Value) then
  begin
    FDiskLimit := Value;
    DoChanged;
  end;
end;

procedure TdxMapControlCacheOptions.SetDiskExpireTime(Value: Int64);
begin
  if (FDiskExpireTime <> Value) then
  begin
    FDiskExpireTime := Value;
    DoChanged;
  end;
end;

procedure TdxMapControlCacheOptions.SetDiskFolder(Value: string);
begin
  FDiskFolder := Value;
  DoChanged;
end;

end.
