{*************************************************************************}
{ TMS TAdvPDFIO                                                           }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2014 - 2015                                       }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvPDFIO;

interface

{$R AdvPDFIO.dcr}

uses
  Classes, AdvPDFLIB;

type
  TOnSetHeaderEvent = procedure(APageRef: TPDFPageItem; APageIndex: Integer) of object;
  TOnSetFooterEvent = procedure(APageRef: TPDFPageItem; APageIndex: Integer) of object;

  TAdvPDFIOComponent = class(TComponent)
  private
    FHeader: TPDFHeader;
    FFooter: TPDFHeader;
    FMetaData: TPDFMetaData;
    FPageLayout: TPDFPageLayout;
    FAutoOpen: boolean;
    FSaveDialogTitle: string;
    FPageSize: TPDFPageSize;
    FOnSetHeader: TOnSetHeaderEvent;
    FOnSetFooter: TOnSetFooterEvent;
    function GetVersionStr: string;
    procedure SetFooter(const Value: TPDFHeader);
    procedure SetHeader(const Value: TPDFHeader);
    procedure SetMetaData(const Value: TPDFMetaData);
    procedure SetVersionStr(const Value: string);
    procedure SetPageLayout(const Value: TPDFPageLayout);
    procedure SetPageSize(const Value: TPDFPageSize);
  protected
    function GetVersionNr: Integer; virtual;
    procedure GeneratePDF(AFileName: string); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    destructor Destroy; override;
    procedure Save(AFileName: string); overload; virtual;
    procedure Save; overload;
    procedure DoSetHeaderEvent(APageRef: TPDFPageItem; APageIndex: Integer);
    procedure DoSetFooterEvent(APageRef: TPDFPageItem; APageIndex: Integer);
  published
    property AutoOpen: Boolean read FAutoOpen write FAutoOpen default False;
    property Footer: TPDFHeader read FFooter write SetFooter;
    property Header: TPDFHeader read FHeader write SetHeader;
    property MetaData: TPDFMetaData read FMetaData write SetMetaData;
    property OnSetHeader: TOnSetHeaderEvent read FOnSetHeader write FOnSetHeader;
    property OnSetFooter: TOnSetFooterEvent read FOnSetFooter write FOnSetFooter;
    property PageLayout: TPDFPageLayout read FPageLayout write SetPageLayout default plPortrait;
    property PageSize: TPDFPageSize read FPageSize write SetPageSize;
    property SaveDialogTitle: string read FSaveDialogTitle write FSaveDialogTitle;
    property Version: string read GetVersionStr write SetVersionStr;
  end;

implementation

uses
  SysUtils, Windows, Dialogs, ShellAPI;

{ TAdvPDFIOComponent }

procedure TAdvPDFIOComponent.Assign(Source: TPersistent);
begin
  if (Source is TAdvPDFIOComponent) then
  begin
    FHeader.Assign((Source as TAdvPDFIOComponent).Header);
    FFooter.Assign((Source as TAdvPDFIOComponent).Footer);
    FMetaData.Assign((Source as TAdvPDFIOComponent).MetaData);
    FPageLayout := ((Source as TAdvPDFIOComponent).PageLayout);
  end;
end;

constructor TAdvPDFIOComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHeader := TPDFHeader.Create;
  FFooter := TPDFHeader.Create;
  FMetaData := TPDFMetaData.Create;
  FPageLayout := plPortrait;
  FPageSize := TPDFPageSize.Create;
end;

destructor TAdvPDFIOComponent.Destroy;
begin
  FMetaData.Free;
  FHeader.Free;
  FFooter.Free;
  FPageSize.Free;
  inherited;
end;

procedure TAdvPDFIOComponent.DoSetFooterEvent(APageRef: TPDFPageItem; APageIndex: Integer);
begin
end;

procedure TAdvPDFIOComponent.DoSetHeaderEvent(APageRef: TPDFPageItem; APageIndex: Integer);
begin
end;

procedure TAdvPDFIOComponent.GeneratePDF(AFileName: string);
begin
  //
end;

function TAdvPDFIOComponent.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(0,0),MakeWord(0,1));
end;

function TAdvPDFIOComponent.GetVersionStr: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

procedure TAdvPDFIOComponent.Save(AFileName: string);
begin
  GeneratePDF(AFileName);
  if AutoOpen then
  begin
    Sleep(250); // wait in case SSD with delayed write is used
    ShellExecute(0, 'open', PChar(AFileName), nil, nil, SW_NORMAL);
  end;

end;

procedure TAdvPDFIOComponent.Save;
var
  sd: TSaveDialog;
  fn: string;
begin
  sd := TSaveDialog.Create(Self);

  try
    sd.Title := SaveDialogTitle;
    sd.Filter := 'PDF files|*.pdf|All files|*.*';

    if sd.Execute then
    begin
      fn := sd.FileName;
      if ExtractFileExt(fn) = '' then
        fn := fn + '.pdf';
      Save(fn);
    end;
  finally
    sd.Free;
  end;
end;

procedure TAdvPDFIOComponent.SetFooter(const Value: TPDFHeader);
begin
  FFooter.Assign(Value);
end;

procedure TAdvPDFIOComponent.SetHeader(const Value: TPDFHeader);
begin
  FHeader.Assign(Value);
end;

procedure TAdvPDFIOComponent.SetMetaData(const Value: TPDFMetaData);
begin
  FMetaData.Assign(Value);
end;

procedure TAdvPDFIOComponent.SetPageLayout(const Value: TPDFPageLayout);
begin
  FPageLayout := Value;
end;

procedure TAdvPDFIOComponent.SetPageSize(const Value: TPDFPageSize);
begin
  FPageSize.Assign(Value);
end;

procedure TAdvPDFIOComponent.SetVersionStr(const Value: string);
begin
  //
end;

end.
