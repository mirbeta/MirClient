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

unit dxRichEdit.Utils.ProgressIndication;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, DateUtils,
  dxCoreClasses,
  dxRichEdit.ServiceManager,
  dxRichEdit.Utils.Types;

type
  { IdxProgressIndicationService }

  IdxProgressIndicationService = interface
  ['{D76289E0-003D-4949-94BD-10E8D3A5FC5C}']
    procedure &Begin(const ADisplayName: string; AMinProgress, AMaxProgress, ACurrentProgress: Integer);
    procedure &End;
    procedure SetProgress(ACurrentProgress: Integer);
  end;

  { IdxRichEditProgressIndicationService }

  IdxRichEditProgressIndicationService = interface(IdxProgressIndicationService)
  ['{331F9078-81CE-4E4A-B9CF-BA76E0C7AC47}']
    procedure SuspendProgressIndication;
    procedure ResumeProgressIndication;
  end;

  { TdxRichEditProgressIndicationService }

  TdxRichEditProgressIndicationService = class(TInterfacedObject,
    IdxRichEditProgressIndicationService,
    IdxProgressIndicationService)
  strict private
    FProvider: IdxServiceProvider;
    FSuspendCount: Integer;
  protected
    function GetIsProgressIndicationSuspended: Boolean; virtual;

    property IsProgressIndicationSuspended: Boolean read GetIsProgressIndicationSuspended;
  public
    constructor Create(const AProvider: IdxServiceProvider);
    procedure &Begin(const ADisplayName: string; AMinProgress: Integer;
      AMaxProgress: Integer; ACurrentProgress: Integer);
    procedure SetProgress(ACurrentProgress: Integer);
    procedure &End;
    procedure SuspendProgressIndication;
    procedure ResumeProgressIndication;
  end;

  { TdxProgressIndication }

  TdxProgressIndication = class(TcxIUnknownObject, IdxProgressIndicationService)
  strict private
  const
    ProgressShowDelay = 500 * OneMillisecond;
    MinIndicationInterval = 50 * OneMillisecond;
  strict private
    FProgressLimit: Integer;
    FProvider: IdxServiceProvider;
    FIndicationTime: TDateTime;
    FDisplayName: string;
    FMinProgress: Integer;
    FProgressRange: Integer;
    FNormalizedProgress: Integer;
    FIndicationState: TdxProgressIndicationState;
  protected
    procedure BeginIndicationCore; virtual;
    procedure IndicateProgressCore; virtual;
    function GetService: IdxProgressIndicationService; virtual;
    function CalculateProgress(AValue: Integer): Integer; virtual;

    property Provider: IdxServiceProvider read FProvider;
  public
    constructor Create(const AProvider: IdxServiceProvider);
    procedure &Begin(const ADisplayName: string; AMinProgress: Integer; AMaxProgress: Integer; ACurrentProgress: Integer); virtual;
    procedure SetProgress(ACurrentProgress: Integer); virtual;
    procedure &End; virtual;
  end;

implementation

uses
  Math;

{ TdxRichEditProgressIndicationService }

constructor TdxRichEditProgressIndicationService.Create(const AProvider: IdxServiceProvider);
begin
  Assert(AProvider <> nil, 'provider');
  inherited Create;
  FProvider := AProvider;
end;

function TdxRichEditProgressIndicationService.GetIsProgressIndicationSuspended: Boolean;
begin
  Result := FSuspendCount <> 0;
end;

procedure TdxRichEditProgressIndicationService.&Begin(const ADisplayName: string; AMinProgress: Integer; AMaxProgress: Integer; ACurrentProgress: Integer);
var
  AService: IdxProgressIndicationService;
begin
  if not IsProgressIndicationSuspended then
  begin
    AService := IdxProgressIndicationService(FProvider.GetService(IdxProgressIndicationService));
    if AService <> nil then
      AService.&Begin(ADisplayName, AMinProgress, AMaxProgress, ACurrentProgress);
  end;
end;

procedure TdxRichEditProgressIndicationService.SetProgress(ACurrentProgress: Integer);
var
  AService: IdxProgressIndicationService;
begin
  if not IsProgressIndicationSuspended then
  begin
    AService := IdxProgressIndicationService(FProvider.GetService(IdxProgressIndicationService));
    if AService <> nil then
      AService.SetProgress(ACurrentProgress);
  end;
end;

procedure TdxRichEditProgressIndicationService.&End;
var
  AService: IdxProgressIndicationService;
begin
  if not IsProgressIndicationSuspended then
  begin
    AService := IdxProgressIndicationService(FProvider.GetService(IdxProgressIndicationService));
    if AService <> nil then
      AService.&End;
  end;
end;

procedure TdxRichEditProgressIndicationService.SuspendProgressIndication;
begin
  Inc(FSuspendCount);
end;

procedure TdxRichEditProgressIndicationService.ResumeProgressIndication;
begin
  Dec(FSuspendCount);
end;

{ TdxProgressIndication }

constructor TdxProgressIndication.Create(const AProvider: IdxServiceProvider);
begin
  Assert(AProvider <> nil);
  inherited Create;
  FProgressLimit := 30;
  FProgressRange := 1;
  FProvider := AProvider;
end;

procedure TdxProgressIndication.&Begin(const ADisplayName: string; AMinProgress: Integer;
  AMaxProgress: Integer; ACurrentProgress: Integer);
begin
  FDisplayName := ADisplayName;
  FMinProgress := AMinProgress;
  FProgressRange := Math.Max(1, AMaxProgress - AMinProgress);
  FNormalizedProgress := CalculateProgress(ACurrentProgress);
  FIndicationTime := Now;
  FIndicationState := TdxProgressIndicationState.Unknown;
end;

procedure TdxProgressIndication.SetProgress(ACurrentProgress: Integer);
var
  AProgress: Integer;
  ANow: TDateTime;
begin
  AProgress := CalculateProgress(ACurrentProgress);

  if FIndicationState = TdxProgressIndicationState.Unknown then
  begin
    ANow := Now;
    if ANow - FIndicationTime >= ProgressShowDelay then
    begin
      if AProgress <= FProgressLimit then
      begin
        FIndicationState := TdxProgressIndicationState.Allowed;
        FNormalizedProgress := AProgress;
        BeginIndicationCore;
        IndicateProgressCore;
        FIndicationTime := ANow;
      end
      else
        FIndicationState := TdxProgressIndicationState.Forbidden;
    end;
  end;

  if AProgress <> FNormalizedProgress then
  begin
    FNormalizedProgress := AProgress;
    if FIndicationState = TdxProgressIndicationState.Allowed then
    begin
      ANow := Now;
      if (ANow - FIndicationTime >= MinIndicationInterval) or (FNormalizedProgress = 100) then
      begin
        IndicateProgressCore;
        FIndicationTime := ANow;
      end;
    end;
  end;
end;

procedure TdxProgressIndication.&End;
var
  AService: IdxProgressIndicationService;
begin
  AService := GetService;
  if AService <> nil then
    AService.&End;
end;

procedure TdxProgressIndication.BeginIndicationCore;
var
  AService: IdxProgressIndicationService;
begin
  AService := GetService;
  if AService <> nil then
    AService.&Begin(FDisplayName, 0, 100, FNormalizedProgress);
end;

procedure TdxProgressIndication.IndicateProgressCore;
var
  AService: IdxProgressIndicationService;
begin
  AService := GetService;
  if AService <> nil then
    AService.SetProgress(FNormalizedProgress);
end;

function TdxProgressIndication.GetService: IdxProgressIndicationService;
begin
  Result := IdxProgressIndicationService(Provider.GetService(IdxProgressIndicationService));
end;

function TdxProgressIndication.CalculateProgress(AValue: Integer): Integer;
begin
  Result := Int64(100 * (Int64(AValue) - FMinProgress)) div FProgressRange;
end;

end.
