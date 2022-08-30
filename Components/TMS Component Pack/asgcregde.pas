{**********************************************************************}
{ TADVCOLUMNGRID component                                             }
{ for Delphi & C++Builder                                              }
{                                                                      }
{ written by TMS Software                                              }
{            copyright © 1996-2012                                     }
{            Email : info@tmssoftware.com                              }
{            Web : http://www.tmssoftware.com                          }
{**********************************************************************}

unit asgcregde;

interface

{$I TMSDEFS.INC}

uses
  AdvGrid, AdvCGrid, Classes, ACGDE,
{$IFDEF DELPHI6_LVL}
  DesignIntf
{$ELSE}
  DsgnIntf
{$ENDIF}
  ;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponentEditor(TAdvColumnGrid,TAdvColumnGridEditor);
end;

end.
