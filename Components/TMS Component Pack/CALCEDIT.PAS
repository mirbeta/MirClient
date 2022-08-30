{************************************************************************}
{ TCALCEDIT component                                                    }
{ for Delphi & C++Builder                                                }
{                                                                        }
{ written by TMS Software                                                }
{            copyright © 1996-2014                                       }
{            Email : info@tmssoftware.com                                }
{            Web : http://www.tmssoftware.com                            }
{                                                                        }
{ The source code is given as is. The author is not responsible          }
{ for any possible damage done due to the use of this code.              }
{ The component can be freely used in any application. The complete      }
{ source code remains property of the author and may not be distributed, }
{ published, given or sold in any form as such. No parts of the source   }
{ code can be included in any other component or application without     }
{ written authorization of the author.                                   }
{ ***********************************************************************}

unit CalcEdit;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, AdvFormula, TypInfo;

type

  ECalcEditError = class(Exception);

  TCalcErrorEvent = procedure(sender: TObject; errorposition:integer) of object;

  TIsCustomFunction = procedure(sender:tobject;var func:string;var match:boolean) of object;
  TCalcCustomFunction = procedure(sender:tobject;var func:string;var param:double) of object;

  TCalcEdit = class(TEdit)
  private
    { Private declarations }
    fExprVars:TExpressionVariables;
    fExpression:string;
    fLastResult:double;
    fResultFormat:string;
    fOnCalcError:TCalcErrorEvent;
    fShowFormula: boolean;
    fHooks:TStringList;
    fCalcCustomFunction: TCalcCustomFunction;
    fIsCustomFunction: TIsCustomFunction;
    fLink: boolean;
    procedure CNCommand(var Msg:TMessage); message CM_CHANGED;
    procedure SetResultFormat(const Value: string);
    function GetText: string;
    procedure SetText(const Value: string);
    procedure SetShowFormula(const Value: boolean);
    function GetVariable(i: integer): double;
    procedure SetVariable(i: integer; const Value: double);
    procedure IsCustom(sender:TObject;var func:string;var match:boolean);
    procedure CalcCustom(sender:TObject;var func:string;var param:double);
    procedure AddCompRef(sender:TObject;compname:string;compref:TCustomEdit);
    procedure SetLink(const Value: boolean);
    function GetFormulaResult: double;
  protected
    { Protected declarations }
    oldhandlers:array[0..MAXVARS] of TNotifyEvent;
    procedure EditChange(sender:TObject);
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Loaded; override;
  public                                      
    { Public declarations }
    constructor Create(aOwner:TComponent); override;
    destructor Destroy; override;
    property Variable[i:integer]:double read GetVariable write SetVariable;
    procedure Calculate;
    procedure Hook;
    procedure UnHook;
    property FormulaResult: double read GetFormulaResult;
  published
    { Published declarations }
    property Link:boolean read fLink write SetLink;    
    property ResultFormat:string read fResultFormat write SetResultFormat;
    property Text:string read GetText write SetText;
    property ShowFormula:boolean read fShowFormula write SetShowFormula;
    property OnCalcError:TCalcErrorEvent read fOnCalcError write fOnCalcError;
    property OnIsCustomFunction:TIsCustomFunction read fIsCustomFunction write fIsCustomFunction;
    property OnCalcCustomFunction:TCalcCustomFunction read fCalcCustomFunction write fCalcCustomFunction;
  end;

  TCalcLabel = class(TLabel)
  private
    fExprVars:TExpressionVariables;
    fShowFormula: boolean;
    fLastResult:double;
    fResultFormat : string;
    fFormula: string;
    fHooks:TStringList;
    fCalcCustomFunction: TCalcCustomFunction;
    fOnCalcError: TCalcErrorEvent;
    fIsCustomFunction: TIsCustomFunction;
    fLink: boolean;
    function GetVariable(i: integer): double;
    procedure SetResultFormat(const Value: string);
    procedure SetShowFormula(const Value: boolean);
    procedure SetVariable(i: integer; const Value: double);
    procedure IsCustom(sender:TObject;var func:string;var match:boolean);
    procedure CalcCustom(sender:TObject;var func:string;var param:double);
    procedure SetFormula(const Value: string);
    procedure AddCompRef(sender:TObject;compname:string;compref:TCustomEdit);
    procedure SetLink(const Value: boolean);
    { Private declarations }
  protected
    { Protected declarations }
    oldhandlers:array[0..MAXVARS] of TNotifyEvent;
    procedure EditChange(sender:TObject);
 public
    { Public declarations }
    constructor Create(aOwner:TComponent); override;
    destructor Destroy; override;
    property Variable[i:integer]:double read GetVariable write SetVariable;
    procedure Calculate;
    procedure Loaded; override;
    procedure Hook;
    procedure UnHook;
  published
    { Published declarations }
    property Link:boolean read fLink write SetLink;
    property ResultFormat:string read fResultFormat write SetResultFormat;
    property ShowFormula:boolean read fShowFormula write SetShowFormula;
    property Formula:string read fFormula write SetFormula;
    property OnCalcError:TCalcErrorEvent read fOnCalcError write fOnCalcError;
    property OnIsCustomFunction:TIsCustomFunction read fIsCustomFunction write fIsCustomFunction;
    property OnCalcCustomFunction:TCalcCustomFunction read fCalcCustomFunction write fCalcCustomFunction;
  end;


implementation

{ TCalcEdit }

procedure TCalcEdit.AddCompRef(sender: TObject; compname: string;
  compref: TCustomEdit);
begin
 if fHooks.IndexOf(compname)=-1 then fHooks.AddObject(compname,compref);
end;

procedure TCalcEdit.CalcCustom(sender: TObject; var func: string;
  var param: double);
begin
  if Assigned(fCalcCustomFunction) then
    fCalcCustomFunction(self,func,param)
  else
    param:=0;
end;

procedure TCalcEdit.Calculate;
var
  af: TAdvFormula;
  res: double;
begin
  if (csDesigning in ComponentState) or (ShowFormula) then
    Exit;
  if (FExpression = '') then
    Exit;
  af := TAdvFormula.Create(Owner);
  af.Expression := FExpression;
  af.V := FExprVars;
  af.IsCustomFunction := IsCustom;
  af.CalcCustomFunction := CalcCustom;
  
  if af.Calc(res) then
  begin
    FLastResult := res;
    inherited Text := Format(fResultFormat,[res]);
  end
  else
  begin
    if Assigned(FOnCalcError) then
      FOnCalcError(self,af.ErrorPosition);
  end;
  af.Free;
end;

procedure TCalcEdit.CNCommand(var Msg: TMessage);
begin
 inherited;
end;

constructor TCalcEdit.Create(aOwner: TComponent);
begin
  inherited;
  FResultFormat := '%.2f';
  FHooks := TStringList.Create;
end;

destructor TCalcEdit.Destroy;
begin
  FHooks.Free;
  inherited;
end;

procedure TCalcEdit.DoEnter;
begin
  inherited;
  inherited Text := FExpression;
end;

procedure TCalcEdit.DoExit;
begin
  FExpression := inherited Text;
  Calculate;
  inherited;
end;

procedure TCalcEdit.EditChange(sender: TObject);
var
  i: Integer;
begin
  Calculate;
  if (Sender is TEdit) then
  begin
    i := FHooks.IndexOf((sender as TEdit).Name);
    if Assigned(oldhandlers[i]) then
      OldHandlers[i](sender);
  end;
end;

function TCalcEdit.GetFormulaResult: double;
begin
  Result := FLastResult;
end;

function TCalcEdit.GetText: string;
begin
  Result := FExpression;
end;

function TCalcEdit.GetVariable(i: integer): double;
begin
  if (i <= 99) and (i >= 0) then
    Result := FExprVars[i]
  else
    raise ECalcEditError.Create('Variable index out of bounds');
end;

procedure TCalcEdit.Hook;
var
  res:double;
  af:TAdvFormula;
  i:integer;
  mc:TComponent;
  NE:TNotifyEvent;
begin
  FHooks.Clear;
  af := TAdvFormula.Create(Owner);
  af.Expression := FExpression;
  af.V := FExprVars;
  af.IsCustomFunction := IsCustom;
  af.CalcCustomFunction := CalcCustom;
  af.OnIsCompRef := AddCompRef;
  af.Calc(res);
  af.Free;

  for i := 1 to fHooks.Count do
  begin
    mc := Owner.FindComponent(fHooks.Strings[i-1]);

    {$IFDEF DELPHI5_LVL}
    if IsPublishedProp(mc,'OnChange') then
    begin
      oldhandlers[i - 1] := TNotifyEvent(GetMethodProp(mc,'OnChange'));
      NE := EditChange;
      SetMethodProp(mc,'OnChange',TMethod(NE));
    end;
    {$ELSE}
    if GetPropInfo(mc.ClassInfo,'OnChange')<>nil then
    begin
      oldhandlers[i - 1] := TNotifyEvent(GetMethodProp(mc,GetPropInfo(mc.ClassInfo, 'OnChange')));
      NE := EditChange;
      SetMethodProp(mc,GetPropInfo(mc.ClassInfo, 'OnChange'),TMethod(NE));
    end;
    {$ENDIF}
    
  end;
end;

procedure TCalcEdit.IsCustom(sender: TObject; var func: string;
  var match: boolean);
begin
  if Assigned(fIsCustomFunction) then
    FIsCustomFunction(self,func,match)
  else
    match := False;
end;

procedure TCalcEdit.Loaded;
begin
  inherited;
  FExpression := Text;
  if FLink and not (csDesigning in ComponentState) then
    Hook;
  Calculate;
end;

procedure TCalcEdit.SetLink(const Value: boolean);
begin
  if (fLink<>Value) and
      not (csLoading in ComponentState) and
      not (csDesigning in ComponentState) then
    begin
     if fLink then Hook else UnHook;
    end;

  fLink := Value;

end;

procedure TCalcEdit.SetResultFormat(const Value: string);
begin
  fResultFormat := Value;
  if not Focused and not (csDesigning in ComponentState) then
    inherited text:=format(fResultFormat,[fLastResult]);
end;

procedure TCalcEdit.SetShowFormula(const Value: boolean);
begin
  fShowFormula := Value;
  if fShowFormula then inherited Text:=fExpression else
   if Focused then Calculate;
end;

procedure TCalcEdit.SetText(const Value: string);
begin
 fExpression:=value;
 if not Focused and not (csDesigning in ComponentState) then Calculate
 else inherited Text:=value;
end;

procedure TCalcEdit.SetVariable(i: integer; const Value: double);
begin
 if (i<=99) and (i>=0) then fExprVars[i]:=value
 else  raise ECalcEditError.Create('Variable index out of bounds');
end;

procedure TCalcEdit.UnHook;
var
 i:integer;
 mc:TComponent;
begin
 for i:=1 to fHooks.Count do
  begin
   mc:=Owner.FindComponent(fHooks.Strings[i-1]);
   {$IFDEF DELPHI5_LVL}
   if IsPublishedProp(mc,'OnChange') then
    begin
     SetMethodProp(mc,'OnChange',TMethod(oldhandlers[i-1]));
    end;
   {$ELSE}
   if GetPropInfo(mc.ClassInfo,'OnChange')<>nil then
    begin
     SetMethodProp(mc.ClassInfo,GetPropInfo(mc.ClassInfo,'OnChange'),TMethod(oldhandlers[i-1]));
    end;
   {$ENDIF}
   {
   (mc as TEdit).OnChange:=oldhandlers[i-1];
   }
  end;
end;

{ TCalcLabel }

procedure TCalcLabel.Calculate;
var
 af:tadvformula;
 res:double;
begin
 if (csDesigning in ComponentState) or (ShowFormula) then exit;
 if (fFormula='') then exit;
 af:=tadvformula.create(Owner);
 af.Expression:=fFormula;
 af.V:=fExprVars;
 af.IsCustomFunction:=IsCustom;
 af.CalcCustomFunction:=CalcCustom;
 if af.Calc(res) then
  begin
    fLastResult:=res;
    Caption:=format(ResultFormat,[res]);
  end
 else
  begin
   if assigned(fOnCalcError) then fOnCalcError(self,af.ErrorPosition);
  end;
 af.Free;
end;

function TCalcLabel.GetVariable(i: integer): double;
begin
 if (i<=99) and (i>=0) then result:=fExprVars[i] else
 raise ECalcEditError.Create('Variable index out of bounds');
end;

procedure TCalcLabel.SetResultFormat(const Value: string);
begin
  fResultFormat := Value;
  if not (csLoading in ComponentState) then Calculate;
end;

procedure TCalcLabel.SetShowFormula(const Value: boolean);
begin
  fShowFormula := Value;
end;

procedure TCalcLabel.SetVariable(i: integer; const Value: double);
begin
 if (i<=99) and (i>=0) then fExprVars[i]:=value
 else  raise ECalcEditError.Create('Variable index out of bounds');
end;

procedure TCalcLabel.IsCustom(sender: TObject; var func: string;
  var match: boolean);
begin
 if assigned(fIsCustomFunction) then fIsCustomFunction(self,func,match) else match:=false;
end;

procedure TCalcLabel.CalcCustom(sender: TObject; var func: string;
  var param: double);
begin
 if assigned(fCalcCustomFunction) then fCalcCustomFunction(self,func,param)
 else param:=0;
end;

procedure TCalcLabel.SetFormula(const Value: string);
begin
  fFormula := Value;
  Calculate;
end;

procedure TCalcLabel.Loaded;
begin
  inherited;
  Calculate;
  if fLink and not (csDesigning in ComponentState) then Hook;
end;

constructor TCalcLabel.Create(aOwner: TComponent);
begin
  inherited;
  fResultFormat:='%.2f';
  fHooks:=TStringList.Create;
end;

destructor TCalcLabel.Destroy;
begin
  fHooks.Free;
  inherited;
end;

procedure TCalcLabel.Hook;
var
 res:double;
 af:TAdvFormula;
 i:integer;
 mc:TComponent;
 NE:TNotifyEvent;

begin

 fHooks.Clear;
 af:=TAdvFormula.Create(Owner);
 af.Expression:=fFormula;
 af.V:=fExprVars;
 af.IsCustomFunction:=IsCustom;
 af.CalcCustomFunction:=CalcCustom;
 af.OnIsCompRef:=AddCompRef;
 af.Calc(res);
 af.Free;

 for i:=1 to fHooks.Count do
  begin
    mc:=Owner.FindComponent(fHooks.Strings[i-1]);

    {$IFDEF DELPHI5_LVL}
    if IsPublishedProp(mc,'OnChange') then
      begin
       oldhandlers[i-1]:=TNotifyEvent(GetMethodProp(mc,'OnChange'));
       NE:=EditChange;
       SetMethodProp(mc,'OnChange',TMethod(NE));
      end;
    {$ELSE}
    if GetPropInfo(mc.ClassInfo,'OnChange')<>nil then
      begin
       oldhandlers[i-1]:=TNotifyEvent(GetMethodProp(mc,GetPropInfo(mc.ClassInfo, 'OnChange')));
       NE:=EditChange;
       SetMethodProp(mc,GetPropInfo(mc.ClassInfo, 'OnChange'),TMethod(NE));
      end;
    {$ENDIF}

   {
   oldhandlers[i-1]:=(mc as TEdit).OnChange;
   (mc as TEdit).OnChange:=EditChange;
   }
  end;
end;

procedure TCalcLabel.UnHook;
var
 i:integer;
 mc:TComponent;
begin
 for i:=1 to fHooks.Count do
  begin
   mc:=Owner.FindComponent(fHooks.Strings[i-1]);
   {$IFDEF DELPHI5_LVL}
   if IsPublishedProp(mc,'OnChange') then
    begin
     SetMethodProp(mc,'OnChange',TMethod(oldhandlers[i-1]));
    end;
   {$ELSE}
   if GetPropInfo(mc.ClassInfo,'OnChange')<>nil then
    begin
     SetMethodProp(mc.ClassInfo,GetPropInfo(mc.ClassInfo,'OnChange'),TMethod(oldhandlers[i-1]));
    end;
   {$ENDIF}

{
   (mc as TEdit).OnChange:=oldhandlers[i-1];
}
  end;
end;

procedure TCalcLabel.AddCompRef(sender: TObject; compname: string;
  compref: TCustomEdit);
begin
 if fHooks.IndexOf(compname)=-1 then
   begin
    oldhandlers[fHooks.Count]:=nil;
    fHooks.AddObject(compname,compref);
   end;
end;

procedure TCalcLabel.EditChange(sender: TObject);
var
 i:integer;
begin
 Calculate;
 if (sender is TCustomEdit) then
  begin
   i:=fHooks.IndexOf((sender as TCustomEdit).Name);
   if assigned(oldhandlers[i]) then oldhandlers[i](sender);
  end;
end;

procedure TCalcLabel.SetLink(const Value: boolean);
begin
  if (fLink<>Value) and
      not (csLoading in ComponentState) and
      not (csDesigning in ComponentState) then
    begin
     if fLink then Hook else UnHook;
    end;
  fLink := Value;
end;

end.
