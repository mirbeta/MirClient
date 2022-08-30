{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPSEvnt;

interface

{$I cxVer.inc}

uses
  Classes, dxPSESys, dxPrnPg, dxPgsDlg, dxPSCore;

type
  TdxSMPageParamsChangedEvent = class(TdxEvent)
  private
    FPrintStyle: TBasedxPrintStyle;
    FUpdateCodes: TdxPrinterPageUpdateCodes;
  public
    constructor Create(ASender: TObject; APrintStyle: TBasedxPrintStyle; AUpdateCodes: TdxPrinterPageUpdateCodes);
    property PrintStyle: TBasedxPrintStyle read FPrintStyle;
    property UpdateCodes: TdxPrinterPageUpdateCodes read FUpdateCodes;
  end;

  TdxSMStyleListChangedEvent = class(TdxEvent);

  TdxStyleListChangedSubscriber = class(TdxEventSubscriber)
  private
    FOnStyleListChanged: TNotifyEvent;
  protected
    procedure DoProcessEvent; override;
  public
    procedure StyleListChanged(Sender: TObject); dynamic;
    property OnStyleListChanged: TNotifyEvent read FOnStyleListChanged write FOnStyleListChanged;
  end;

  TdxPageParamsChangedSubscriber = class(TdxEventSubscriber)
  private
    FOnPageParamsChanged: TdxPageParamsChangedEvent;
  protected
    procedure DoProcessEvent; override;
  public
    procedure PageParamsChanged(Sender: TObject; AStyle: TBasedxPrintStyle; AUpdateCodes: TdxPrinterPageUpdateCodes); dynamic;
    property OnPageParamsChanged: TdxPageParamsChangedEvent read FOnPageParamsChanged write FOnPageParamsChanged;
  end;

  TdxComponentPrinterEvent = class(TdxEvent)
  private
    function GetComponentPrinter: TdxComponentPrinter;
  public
    property ComponentPrinter: TdxComponentPrinter read GetComponentPrinter;
  end;

  TdxPSPrintEvent = class(TdxComponentPrinterEvent)
  private
    FPageCount: Integer;
    FPageIndex: Integer;
    FReportLink: TBasedxReportLink;
    FStage: TdxPSPrintStage;
  public
    constructor Create(Sender: TObject; AReportLink: TBasedxReportLink;
      APageIndex, APageCount: Integer; AStage: TdxPSPrintStage);

    property PageCount: Integer read FPageCount;
    property PageIndex: Integer read FPageIndex;
    property ReportLink: TBasedxReportLink read FReportLink;
    property Stage: TdxPSPrintStage read FStage;
  end;

  TdxPSBuildEvent = class(TdxComponentPrinterEvent)
  private
    FPercentCompleted: Double;
    FReportLink: TBasedxReportLink;
    FStage: TdxPSBuildStage;
  public
    constructor Create(Sender: TObject; AReportLink: TBasedxReportLink;
      const APercentCompleted: Double; AStage: TdxPSBuildStage);

    property PercentCompleted: Double read FPercentCompleted;
    property ReportLink: TBasedxReportLink read FReportLink;
    property Stage: TdxPSBuildStage read FStage;
  end;

  TdxPSPrintReportSubscriber = class(TdxEventSubscriber)
  private
    FOnEndPrint: TdxReportLinkNotifyEvent;
    FOnProgressPrint: TdxNewPageEvent;
    FOnStartPrint: TdxStartPrintEvent;
  protected
    procedure DoProcessEvent; override;
  public
    procedure EndPrint(Sender: TObject; AReportLink: TBasedxReportLink); dynamic;
    procedure ProgressPrint(Sender: TObject; AReportLink: TBasedxReportLink; APageIndex: Integer); dynamic;
    procedure StartPrint(Sender: TObject; AReportLink: TBasedxReportLink; APageCount: Integer); dynamic;

    property OnEndPrint: TdxReportLinkNotifyEvent read FOnEndPrint write FOnEndPrint;
    property OnProgressPrint: TdxNewPageEvent read FOnProgressPrint write FOnProgressPrint;
    property OnStartPrint: TdxStartPrintEvent read FOnStartPrint write FOnStartPrint;
  end;

  TdxPSBuildReportSubscriber = class(TdxEventSubscriber)
  private
    FOnEndGenerateReport: TdxReportLinkNotifyEvent;
    FOnGenerateReportProgress: TdxGenerateReportProgressEvent;
    FOnStartGenerateReport: TdxReportLinkNotifyEvent;
  protected
    procedure DoProcessEvent; override;
  public
    procedure EndBuild(Sender: TObject; AReportLink: TBasedxReportLink); dynamic;
    procedure ProgressBuild(Sender: TObject; AReportLink: TBasedxReportLink; APercentCompleted: Double); dynamic;
    procedure StartBuild(Sender: TObject; AReportLink: TBasedxReportLink); dynamic;

    property OnEndGenerateReport: TdxReportLinkNotifyEvent read FOnEndGenerateReport  write FOnEndGenerateReport;
    property OnGenerateReportProgress: TdxGenerateReportProgressEvent read FOnGenerateReportProgress write FOnGenerateReportProgress;
    property OnStartGenerateReport: TdxReportLinkNotifyEvent read FOnStartGenerateReport write FOnStartGenerateReport;
  end;

  TdxHFTextEntriesChangedEvent = class(TdxEvent);

  TdxNoParamsEvent = procedure of object;

  TdxHFTextEntriesChangedSubscriber = class(TdxEventSubscriber)
  private
    FOnHFTextEntriesChanged: TdxNoParamsEvent;
  protected
    procedure DoProcessEvent; override;
  public
    property OnHFTextEntriesChanged: TdxNoParamsEvent read FOnHFTextEntriesChanged  write FOnHFTextEntriesChanged;
  end;

  TdxHFTextEntryChooseEvent = class(TdxEvent)
  private
    FEntry: string;
  public
    constructor Create(Sender: TObject; const AEntry: string);
    property Entry: string read FEntry;
  end;

  TdxHFTextEntryEvent = procedure(Sender: TObject; const AEntry: string) of object;

  TdxHFTextEntryChooseSubscriber = class(TdxEventSubscriber)
  private
    FOnHFTextEntryChoose: TdxHFTextEntryEvent;
  protected
    procedure DoProcessEvent; override;
  public
    property OnHFTextEntryChoose: TdxHFTextEntryEvent read FOnHFTextEntryChoose write FOnHFTextEntryChoose;
  end;

implementation

{ TdxSMPageParamsChangedEvent }

constructor TdxSMPageParamsChangedEvent.Create(ASender: TObject;
  APrintStyle: TBasedxPrintStyle; AUpdateCodes: TdxPrinterPageUpdateCodes);
begin
  inherited Create(ASender);
  FPrintStyle := APrintStyle;
  FUpdateCodes := AUpdateCodes;
end;

{ TdxStyleListChangedSubscriber }

procedure TdxStyleListChangedSubscriber.DoProcessEvent;
begin
  if ActiveEvent is TdxSMStyleListChangedEvent then
    StyleListChanged(ActiveEvent.Sender)
end;

procedure TdxStyleListChangedSubscriber.StyleListChanged(Sender: TObject);
begin
  if Assigned(FOnStyleListChanged) then FOnStyleListChanged(Sender);
end;

{ TdxPageParamsChangedSubscriber }

procedure TdxPageParamsChangedSubscriber.DoProcessEvent;
begin
  if ActiveEvent is TdxSMPageParamsChangedEvent then
    with TdxSMPageParamsChangedEvent(ActiveEvent) do
      PageParamsChanged(PrintStyle.PrinterPage, PrintStyle, UpdateCodes);
end;

procedure TdxPageParamsChangedSubscriber.PageParamsChanged(Sender: TObject;
  AStyle: TBasedxPrintStyle; AUpdateCodes: TdxPrinterPageUpdateCodes);
begin
  if Assigned(FOnPageParamsChanged) then
    FOnPageParamsChanged(TdxPrinterPage(Sender), AStyle, AUpdateCodes);
end;

{ TdxComponentPrinterEvent }

function TdxComponentPrinterEvent.GetComponentPrinter: TdxComponentPrinter;
begin
  Result := TdxComponentPrinter(Sender);
end;

{ TdxPSPrintEvent }

constructor TdxPSPrintEvent.Create(Sender: TObject; AReportLink: TBasedxReportLink;
  APageIndex, APageCount: Integer; AStage: TdxPSPrintStage);
begin
  inherited Create(Sender);
  FReportLink := AReportLink;
  FPageIndex := APageIndex;
  FPageCount := APageCount;
  FStage := AStage;
end;

{ TdxPSBuildEvent }

constructor TdxPSBuildEvent.Create(Sender: TObject; AReportLink: TBasedxReportLink;
  const APercentCompleted: Double; AStage: TdxPSBuildStage);
begin
  inherited Create(Sender);
  FReportLink := AReportLink;
  FPercentCompleted := APercentCompleted;
  FStage := AStage;
end;

{ TdxPSBuildReportSubscriber }

procedure TdxPSBuildReportSubscriber.DoProcessEvent;
begin
  if ActiveEvent is TdxPSBuildEvent then
    with TdxPSBuildEvent(ActiveEvent) do
      case Stage of
        bsStart:
          StartBuild(ComponentPrinter, ReportLink);
        bsProgress:
          ProgressBuild(ComponentPrinter, ReportLink, PercentCompleted);
        bsEnd:
          EndBuild(ComponentPrinter, ReportLink);
      end;
end;

procedure TdxPSBuildReportSubscriber.EndBuild(Sender: TObject;
  AReportLink: TBasedxReportLink);
begin
  if Assigned(FOnEndGenerateReport) then
    FOnEndGenerateReport(Sender, AReportLink);
end;

procedure TdxPSBuildReportSubscriber.ProgressBuild(Sender: TObject;
  AReportLink: TBasedxReportLink; APercentCompleted: Double);
begin
  if Assigned(FOnGenerateReportProgress) then
    FOnGenerateReportProgress(Sender, AReportLink, APercentCompleted);
end;

procedure TdxPSBuildReportSubscriber.StartBuild(Sender: TObject;
  AReportLink: TBasedxReportLink);
begin
  if Assigned(FOnStartGenerateReport) then
    FOnStartGenerateReport(Sender, AReportLink);
end;

{ TdxPSPrintReportSubscriber }

procedure TdxPSPrintReportSubscriber.DoProcessEvent;
begin
  if ActiveEvent is TdxPSPrintEvent then
    with TdxPSPrintEvent(ActiveEvent) do
      case Stage of
        psStart:
          StartPrint(ComponentPrinter, ReportLink, PageCount);
        psProgress:
          ProgressPrint(ComponentPrinter, ReportLink, PageIndex);
        psEnd:
          EndPrint(ComponentPrinter, ReportLink);
      end;
end;

procedure TdxPSPrintReportSubscriber.EndPrint(Sender: TObject;
  AReportLink: TBasedxReportLink);
begin
  if Assigned(FOnEndPrint) then FOnEndPrint(Sender, AReportLink);
end;

procedure TdxPSPrintReportSubscriber.ProgressPrint(Sender: TObject;
  AReportLink: TBasedxReportLink; APageIndex: Integer);
begin
  if Assigned(FOnProgressPrint) then
    FOnProgressPrint(Sender, AReportLink, APageIndex);
end;

procedure TdxPSPrintReportSubscriber.StartPrint(Sender: TObject;
  AReportLink: TBasedxReportLink; APageCount: Integer);
begin
  if Assigned(FOnStartPrint) then
    FOnStartPrint(Sender, AReportLink, APageCount);
end;

{ TdxHFTextEntriesChangedSubscriber }

procedure TdxHFTextEntriesChangedSubscriber.DoProcessEvent;
begin
  if ActiveEvent is TdxHFTextEntriesChangedEvent then
    if Assigned(FOnHFTextEntriesChanged) then FOnHFTextEntriesChanged;
end;

{ TdxHFTextEntryChooseEvent }

constructor TdxHFTextEntryChooseEvent.Create(Sender: TObject; const AEntry: string);
begin
  inherited Create(Sender);
  FEntry := AEntry;
end;

{ TdxHFTextEntryChooseSubscriber }

procedure TdxHFTextEntryChooseSubscriber.DoProcessEvent;
begin
  if ActiveEvent is TdxHFTextEntryChooseEvent then
    if Assigned(FOnHFTextEntryChoose) then
      FOnHFTextEntryChoose(ActiveEvent.Sender, TdxHFTextEntryChooseEvent(ActiveEvent).Entry);
end;

end.
