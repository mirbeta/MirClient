{*************************************************************************}
{ TDBHTMLabel component                                                   }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{            copyright © 1999-2013                                        }
{            Email : info@tmssoftware.com                                 }
{            Website : http://www.tmssoftware.com/                        }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit dbhtmlab;

{$I TMSDEFS.INC}

interface

uses
  htmlabel, DB, Classes, SysUtils, Windows, Messages, Controls, Graphics,
  PictureContainer, ImgList
  , Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 4; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // 1.3.1.0 : New : support for customizing bullets in HTML UL lists
  // 1.4.0.0 : New : Support for PNG images via images in associated PictureContainer


type
  TDBHTMLabel = class;

  TGetDataEvent = procedure (Sender:TObject; Tag:string;var Data:string) of object;

  THTMLDataLink = class(TDataLink)
  private
    FLabel:TDBHTMLabel;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(distance:integer); override;
    procedure RecordChanged(Field: TField); override;
  public
    constructor Create(ALabel: TDBHTMLabel);
    destructor Destroy; override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBHTMLabel = class(THTMLabel)
  private
    FDataLink:THTMLDataLink;
    FDBHTMLText:string;
    FSelectFontColor: TColor;
    FNormalFontColor: TColor;
    FOnGetData: TGetDataEvent;
    FOnTransformData: TGetDataEvent;
    function GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure SetSelectFontColor(const Value: tColor);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    function GetVersionNr: Integer; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateDisplText; override;
    procedure Loaded; override;
    function HTMLPaint(Canvas: TCanvas;s:string;fr:TRect;
                       FImages:TImageList;
                       XPos,YPos,Focuslink,Hoverlink,ShadowOffset: Integer;
                       Checkhotspot,Checkheight,Print,Selected,Blink,Hoverstyle: Boolean;
                       Resfactor: Double;
                       UrlColor,HoverColor,HoverFontColor,ShadowColor: TColor;
                       var AnchorVal,StripVal,FocusAnchor: string;
                       var xsize,ysize,Hyperlinks,Mouselink: Integer;
                       var hoverrect:TRect):Boolean; override;
    function HTMLDBReplace(s:string; DataSet: TDataSet):string;
    procedure DBUpdate;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateHTML;
    function GetDisplText:string; override;
  published
    property Datasource:  TDataSource read GetDataSource write SetDataSource;
    property SelectFontColor: TColor read FSelectFontColor write SetSelectFontColor;
    property OnGetData: TGetDataEvent read FOnGetData write FOnGetData;
    property OnTransformData: TGetDataEvent read FOnTransformData write FOnTransformData;
  end;

implementation

uses
  CommCtrl, ShellApi;


type
  {$IFDEF DELPHIXE_LVL}
  LInteger = LONG_PTR;
  LIntParam = LPARAM;
  {$ENDIF}
  {$IFNDEF DELPHIXE_LVL}
  LInteger = Integer;
  LIntParam = Integer;
  {$ENDIF}
  IntPtr = Pointer;


{$DEFINE DBAWARE}
{$DEFINE REMOVEDRAW}
{$DEFINE REMOVESTRIP}
{$I HTMLENGO.PAS}

function TDBHTMLabel.HTMLDBReplace(s:string; DataSet: TDataSet):string;
var
  BeforeTag,AfterTag,Fld,DBFld:string;
  i,j,idx: Integer;
  ms: TMemoryStream;
  Picture: TPicture;

begin
  i := 1;

  while i < ImageCache.Count do
  begin
    if ImageCache.Items[i - 1].IsDB then
    begin
      ImageCache.Items[i - 1].Free;
      ImageCache.Delete(i - 1);
    end
    else
      Inc(i);
  end;

  idx := 0;

  BeforeTag := '';
  while VarPos('<#',s,i)>0 do
  begin
    BeforeTag := BeforeTag + Copy(s,1,i - 1); //part prior to the tag
    AfterTag := Copy(s,i,Length(s)); //part after the tag

    j := Pos('>',AfterTag);
    Fld := Copy(AfterTag,1,j - 1);

    Delete(Fld,1,2);
    Delete(s,1,i + j - 1);

    DBFld := '';

    if Assigned(FOnGetData) then
      FOnGetData(Self,Fld,DBFld);

    if Assigned(DataSet) and (DBFld = '') then
    begin
      if DataSet.Active then
      begin
        if Assigned(DataSet.FindField(Fld)) then
        if DataSet.FieldByName(Fld).IsBlob then
        begin
          ms := TMemoryStream.Create;

          if DataSet.FieldByName(fld).DataType = ftGraphic then
          begin
            Picture := TPicture.Create;
            Picture.Assign(DataSet.FieldByName(fld));
            Picture.Graphic.SaveToStream(ms);
            Picture.Free;
          end
          else
            TBlobField(DataSet.FieldByName(fld)).SaveToStream(ms);

          ms.Position := 0;
          if ms.Size > 0 then
          with ImageCache.AddPicture do
          begin
            LoadFromStream(ms);
            ID := 'FILE://DB_'+inttostr(idx);
            IsDB := true;
            inc(Idx);
            DBFld := ID;
          end;

          ms.Free;
        end
        else
          DBFld := DataSet.FieldByName(fld).DisplayText;
      end
      else
        DBfld := '('+fld+')';
    end
    else
     if DBFld='' then DBfld := '('+fld+')';

    if Assigned(FOnTransformData) then
      FOnTransformData(Self,Fld,DBFld);

    BeforeTag := BeforeTag + DBFld;
  end;

  Result := Beforetag + s;
end;


{ TDBHTMLabel }

constructor TDBHTMLabel.Create(AOwner: TComponent);
begin
  inherited;
  FDataLink := THTMLDataLink.Create(self);
  ControlStyle := ControlStyle + [csReplicatable];
  FSelectFontColor := clBlack;
end;

procedure TDBHTMLabel.DBUpdate;
begin
  if Assigned(Datasource) then
  begin
    if Assigned(DataSource.DataSet) then
    begin
      if DataSource.DataSet.Active then
      begin
        FDBHTMLText := HTMLDBReplace(inherited GetDisplText,Datasource.Dataset);
      end;
    end;
  end
  else
  begin
    FDBHTMLText := HTMLDBReplace(inherited GetDisplText,nil);
  end;
end;

destructor TDBHTMLabel.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited;
end;

function TDBHTMLabel.GetDataSource: TDataSource;
begin
 Result := FDataLink.DataSource;
end;

procedure TDBHTMLabel.UpdateDisplText;
begin
  DBUpdate;
  inherited;
end;

function TDBHTMLabel.GetDisplText: string;
begin
  Result := FDBHTMLText;
end;

procedure TDBHTMLabel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TDBHTMLabel.SetDataSource(const Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  DBUpdate;
end;

procedure TDBHTMLabel.WMPaint(var Message: TWMPaint);
begin
  DBUpdate;

  if  not (csPaintCopy in ControlState) then
  begin
    Canvas.Font.Color := FSelectFontColor;
    inherited;
  end
  else
  begin
    Canvas.Font.Color := FNormalFontColor;
    inherited;
  end;
end;

procedure TDBHTMLabel.SetSelectFontColor(const Value: tColor);
begin
  FSelectFontColor := Value;
  Invalidate;
end;

procedure TDBHTMLabel.Loaded;
begin
  inherited;
  FNormalFontColor := self.Font.Color;
end;

procedure TDBHTMLabel.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LInteger(FDataLink);
end;


function TDBHTMLabel.HTMLPaint(Canvas: TCanvas; s: string; fr: TRect;
  FImages: TImageList; xpos, ypos, focuslink, hoverlink,
  ShadowOffset: integer; Checkhotspot, checkheight, print, selected, blink,
  HoverStyle: boolean; resfactor: double; urlcolor, hovercolor,
  HoverFontColor, ShadowColor: TColor; var anchorval, stripval,
  focusanchor: string; var xsize, ysize, hyperlinks, mouselink: integer;
  var hoverrect: TRect): boolean;
begin
  Result := HTMLDrawEx(Canvas,s,fr,FImages,XPos,ypos,-1,HoverLink,ShadowOffset,checkhotspot,checkheight,print,selected,blink,
                       hoverstyle,not Ellipsis,resfactor,urlcolor,hovercolor,hoverfontcolor,shadowcolor,anchorval,stripval,focusanchor,
                       xsize,ysize,hyperlinks,mouselink,Hoverrect,ImageCache,PictureContainer,0);
end;

procedure TDBHTMLabel.UpdateHTML;
begin
  DBUpdate;
end;

function TDBHTMLabel.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

{ THTMLDataLink }

procedure THTMLDataLink.ActiveChanged;
begin
  inherited;
  fLabel.DBUpdate;
  fLabel.Invalidate;
end;

constructor THTMLDataLink.Create(ALabel: TDBHTMLabel);
begin
 inherited Create;
 fLabel:=aLabel;
end;

procedure THTMLDataLink.DataSetChanged;
begin
  inherited;
  fLabel.DBUpdate;
end;

procedure THTMLDataLink.DataSetScrolled(distance: integer);
begin
  inherited;
  FLabel.DBUpdate;
end;

destructor THTMLDataLink.Destroy;
begin
  inherited;
end;

procedure THTMLDataLink.RecordChanged(Field: TField);
begin
  inherited;
  FLabel.DBUpdate;
  FLabel.Invalidate;
end;



end.
