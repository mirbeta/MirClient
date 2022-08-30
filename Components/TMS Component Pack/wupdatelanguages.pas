{*******************************************************************}
{ TWEBUPDATE Wizard component                                       }
{ for Delphi & C++Builder                                           }
{                                                                   }
{ written by                                                        }
{    TMS Software                                                   }
{    copyright � 1998 - 2015                                        }
{    Email : info@tmssoftware.com                                   }
{    Web   : http://www.tmssoftware.com                             }
{                                                                   }
{ The source code is given as is. The author is not responsible     }
{ for any possible damage done due to the use of this code.         }
{ The component can be freely used in any application. The source   }
{ code remains property of the writer and may not be distributed    }
{ freely as such.                                                   }
{*******************************************************************}

unit WUpdateLanguages;

interface

uses
  Classes, WUpdateWiz;

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TWebUpdateWizardEnglish = class(TWebUpdateWizardLanguage)
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TWebUpdateWizardDutch = class(TWebUpdateWizardLanguage)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TWebUpdateWizardFrench = class(TWebUpdateWizardLanguage)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TWebUpdateWizardGerman = class(TWebUpdateWizardLanguage)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TWebUpdateWizardPortugese = class(TWebUpdateWizardLanguage)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TWebUpdateWizardSpanish = class(TWebUpdateWizardLanguage)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TWebUpdateWizardDanish = class(TWebUpdateWizardLanguage)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TWebUpdateWizardItalian = class(TWebUpdateWizardLanguage)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TWebUpdateWizardNorwegian = class(TWebUpdateWizardLanguage)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TWebUpdateWizardHungarian = class(TWebUpdateWizardLanguage)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TWebUpdateWizardSwedish = class(TWebUpdateWizardLanguage)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TWebUpdateWizardCzech = class(TWebUpdateWizardLanguage)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TWebUpdateWizardPolish = class(TWebUpdateWizardLanguage)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TWebUpdateWizardCatalan = class(TWebUpdateWizardLanguage)
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ TWebUpdateWizardDutch }

constructor TWebUpdateWizardDutch.Create(AOwner: TComponent);
begin
  inherited;
  Welcome := 'Druk start om te beginnen met controleren voor applicatie updates ...';
  StartButton := 'Start';
  NextButton := 'Volgende';
  ExitButton := 'Verlaten';
  CancelButton := 'Annuleren';
  RestartButton := 'Herstarten';
  GetUpdateButton := 'Update';
  NewVersionFound := 'Nieuwe version gevonden';
  NewVersion := 'Nieuwe versie';
  NoNewVersionAvail := 'Geen nieuwe versie beschikbaar.';
  NewVersionAvail := 'Nieuwe versie beschikbaar.';
  CurrentVersion := 'Huidige versie';
  NoFilesFound := 'Geen bestanden gevonden voor update';
  NoUpdateOnServer := 'geen update gevonden op server ...';
  CannotConnect := 'Er kan geen verbinding met de update server tot stand gebracht worden of';
  WhatsNew := 'Nieuw in update';
  License := 'Licentie overeenkomst';
  AcceptLicense := 'Ik aanvaard';
  NotAcceptLicense := 'Ik aanvaard niet';
  ComponentsAvail := 'Beschikbare applicatie componenten';
  DownloadingFiles := 'Downloaden bestanden';
  CurrentProgress := 'Vooruitgang huidig bestand';
  TotalProgress := 'Totale vooruitgang';
  UpdateComplete := 'Update volledig ...';
  RestartInfo := 'Druk Herstarten om de nieuwe versie te starten.';
  WhatsNewPopup := 'Bekijken in kladblok';
  LicensePopup := 'Bekijken in kladblok';
end;

{ TWebUpdateWizardFrench }

constructor TWebUpdateWizardFrench.Create(AOwner: TComponent);
begin
  inherited;
  Welcome := 'Cliquez sur V�rifier pour rechercher d''�ventuelles mises � jour ...';
  StartButton := 'V�rifier';
  NextButton := 'Suivant';
  ExitButton := 'Quitter';
  CancelButton := 'Annuler';
  RestartButton := 'Red�marrer';
  GetUpdateButton := 'Mettre � jour';
  NewVersionFound := 'Une nouvelle version est disponible';
  NewVersion := 'Nouvelle version';
  NoNewVersionAvail := 'Pas de nouvelle version disponible.';
  NewVersionAvail := 'Nouvelle version disponible.';
  CurrentVersion := 'Version actuelle';
  NoFilesFound := 'pas de mises � jour disponible';
  NoUpdateOnServer := 'Aucune mise � jour disponible ...';
  CannotConnect := 'Connexion impossible avec le serveur ou';
  WhatsNew := 'Nouveaut�s';
  License := 'Informations de license';
  AcceptLicense := 'J''accepte';
  NotAcceptLicense := 'Je refuse';
  ComponentsAvail := 'Composants d''application disponible';
  DownloadingFiles := 'T�l�chargement des fichiers';
  CurrentProgress := 'Progression du fichier';
  TotalProgress := 'Progression totale';
  UpdateComplete := 'Mise � jour termin�e';
  RestartInfo := 'Cliquez sur Red�marrer pour lancer la nouvelle version';
  FailedDownload := 'Echec de la mise � jour';
  LicensePopup := 'Voir avec Notepad';
  WhatsNewPopup := 'Voir avec Notepad';
end;

{ TWebUpdateWizardGerman }

constructor TWebUpdateWizardGerman.Create(AOwner: TComponent);
begin
  inherited;
  Welcome := 'Klicken Sie auf Starten, um mit der Updatepr�fung zu beginnen...';
  StartButton := 'Start';
  NextButton := 'Weiter';
  ExitButton := 'Schlie�en';
  RestartButton := 'Neu starten';
  CancelButton := 'Abbrechen';
  GetUpdateButton := 'Weiter';
  NewVersionFound := 'Es wurde eine neue Version gefunden';
  NewVersion := 'Neue Version';
  NoNewVersionAvail := 'Keine neue Version verf�gbar.';
  NewVersionAvail := 'Es ist eine neue Version verf�gbar';
  CurrentVersion := 'Ihre Version';
  NoFilesFound := 'Auf dem Server wurden keine Dateien gefunden.';
  NoUpdateOnServer := 'Kein Update vorhanden auf Server ...';
  CannotConnect := 'Die Verbindung zum Server konnte nicht hergestellt werden';
  WhatsNew := 'Was ist neu?';
  License := 'Lizenzvereinbarung';
  AcceptLicense := 'Ich akzeptiere die Vereinbarung' ;
  NotAcceptLicense := 'Ich lehne die Vereinbarung ab';
  ComponentsAvail := 'Verf�gbare Anwendungskomponenten';
  DownloadingFiles := 'Lade Dateien';
  CurrentProgress := 'Dateidownload:';
  TotalProgress := 'Gesamter Updateverlauf:';
  UpdateComplete := 'Der Download des Updates ist komplett ...';
  RestartInfo := 'Best�tigen Sie den Neustart,'+chr(13)+chr(10)+'um das Update zu starten.';
  FailedDownload := 'Fehler w�hrend download update dateien';
  LicensePopup := 'Zeigen in Notepad';
  WhatsNewPopup := 'Zeigen in Notepad';
end;

{ TWebUpdateWizardPortugese }

constructor TWebUpdateWizardPortugese.Create(AOwner: TComponent);
begin
  inherited;
  Welcome := 'Clique iniciar para verificar se h� novas atualiza��es...';
  StartButton := 'Iniciar';
  NextButton := 'Pr�ximo';
  ExitButton := 'Sair';
  CancelButton := 'Cancelar';
  RestartButton := 'Reiniciar';
  GetUpdateButton := 'Obter atualiza��o';
  NewVersionFound := 'Nova vers�o encontrada';
  NewVersion := 'Nova vers�o';
  NoNewVersionAvail := 'N�o h� novas vers�es dispon�veis.';
  NewVersionAvail := 'Nova vers�o dispon�vel.';
  CurrentVersion := 'Vers�o atual';
  NoFilesFound := 'Nenhum arquivo encontrado para a atualiza��o';
  NoUpdateOnServer := 'Nenhuma atualiza��o encontrada no servidor...';
  CannotConnect := 'N�o foi poss�vel conectar ao servidor de atualiza��o ou';
  WhatsNew := 'O que h� de novo';
  License := 'Contrato de licen�a';
  AcceptLicense := 'Aceito';
  NotAcceptLicense := 'N�o aceito';
  ComponentsAvail := 'Componentes da aplica��o dispon�veis';
  DownloadingFiles := 'Fazendo o download dos arquivos';
  CurrentProgress := 'Progresso do arquivo atual';
  TotalProgress := 'Progresso total';
  UpdateComplete := 'Atualiza��o conclu�da...';
  RestartInfo := 'Clique reiniciar para iniciar a aplica��o atualizada.';
  FailedDownload := 'Falha no download da atualiza��o';
  LicensePopup := 'Exibir no Notepad';
  WhatsNewPopup := 'Exibir no Notepad';
end;

{ TWebUpdateWizardSpanish }

constructor TWebUpdateWizardSpanish.Create(AOwner: TComponent);
begin
  inherited;
  Welcome := 'Presione iniciar para buscar actualizaciones disponibles de la aplicaci�n ...';
  StartButton := 'Iniciar';
  NextButton := 'Siguiente';
  ExitButton := 'Salir';
  CancelButton := 'Cancelar';
  RestartButton := 'Reiniciar';
  GetUpdateButton := 'Obtener actualizaci�n';
  NewVersionFound := 'Nueva versi�n encontrada';
  NewVersion := 'Nueva versi�n';
  NoNewVersionAvail := 'No hay una nueva versi�n disponible.';
  NewVersionAvail := 'Nueva versi�n disponible.';
  CurrentVersion := 'Versi�n actual';
  NoFilesFound := 'No se encontraron archivos para actualizar';
  NoUpdateOnServer := 'no se encontr� una nueva actualizaci�n en el servidor ...';
  CannotConnect := 'No se puedo establecer la conexi�n con el servidor de actualizaciones o ';
  WhatsNew := 'Lo nuevo';
  License := 'Acuerdo de licenciamiento';
  AcceptLicense := 'Acepto';
  NotAcceptLicense := 'No acepto';
  ComponentsAvail := 'Componentes de la aplicaci�n disponibles';
  DownloadingFiles := 'Descargando archivos';
  CurrentProgress := 'Progreso de archivo actual';
  TotalProgress := 'Progreso total';
  UpdateComplete := 'Actualizaci�n completada ...';
  RestartInfo := 'Presione reiniciar para ejecutar la aplicaci�n actualizada.';
end;

{ TWebUpdateWizardDanish }

constructor TWebUpdateWizardDanish.Create(AOwner: TComponent);
begin
  inherited;
  Welcome := 'Tryk p� Start-knappen for at checke for applikationsopdateringer ...';
  StartButton := 'Start';
  NextButton := 'N�ste';
  ExitButton := 'Afslut';
  CancelButton := 'Fortryd';
  RestartButton := 'Genstart';
  GetUpdateButton := 'Hent opdatering';
  NewVersionFound := 'Ny version blev fundet';
  NewVersion := 'Ny version';
  NoNewVersionAvail := 'Ingen ny version tilg�ngelig.';
  NewVersionAvail := 'Ny version tilg�ngelig.';
  CurrentVersion := 'Nuv�rende version';
  NoFilesFound := 'Ingen opdaterbare filer blev fundet';
  NoUpdateOnServer := 'ingen opdatering blev fundet p� serveren ...';
  CannotConnect := 'Kunne ikke f� kontakt til opdateringsserveren eller';
  WhatsNew := 'Hvad er nyt?';
  License := 'Licensaftale';
  AcceptLicense := 'Jeg accepterer';
  NotAcceptLicense := 'Jeg accepterer ikke';
  ComponentsAvail := 'Tilg�ngelige applikationskomponenter';
  DownloadingFiles := 'Downloader filer';
  CurrentProgress := 'Nuv�rende filforl�b';
  TotalProgress := 'Total filforl�b';
  UpdateComplete := 'Opdatering fuldf�rt ...';
  RestartInfo := 'Tryk p� genstart for at starte den opdaterede applikation.';
end;

{ TWebUpdateWizardItalian }

constructor TWebUpdateWizardItalian.Create(AOwner: TComponent);
begin
  inherited;
  Welcome := 'Premi Inizia per verificare la disponibilit� di aggiornamenti dell''applicazione...';
  StartButton := 'Inizia';
  NextButton := 'Avanti';
  ExitButton := 'Esci';
  CancelButton := 'Annulla';
  RestartButton := 'Riavvia';
  GetUpdateButton := 'Scarica l''aggiornamento';
  NewVersionFound := 'Trovata una nuova versione';
  NewVersion := 'Nuova versione';
  NoNewVersionAvail := 'Nessuna nuova versione disponibile.';
  NewVersionAvail := 'Nuova versione disponibile.';
  CurrentVersion := 'Versione corrente';
  NoFilesFound := 'file non trovati per l''aggiornamento';
  NoUpdateOnServer := 'non c''� un nuovo aggiornamento sul server...';
  CannotConnect := 'Impossibile stabilire la connessione con il server o ';
  WhatsNew := 'Novit�';
  License := 'Accordo di licenza';
  AcceptLicense := 'Accetto';
  NotAcceptLicense := 'Non accetto';
  ComponentsAvail := 'Componenti dell''applicazione disponibil';
  DownloadingFiles := 'Scarico i file';
  CurrentProgress := 'Avanzamento del file corrente';
  TotalProgress := 'Avanzamento complessivo';
  UpdateComplete := 'Aggiornamento completo...';
  RestartInfo := 'Premi riavvia per eseguire l''applicazione aggiornata.';
end;

{ TWebUpdateWizardNorwegian }

constructor TWebUpdateWizardNorwegian.Create(AOwner: TComponent);
begin
  inherited;
  Welcome := 'Klikk Start for � se etter tilgjengelige oppdateringer av programmet...';
  StartButton := 'Start';
  NextButton := 'Neste';
  ExitButton := 'Avslutt';
  CancelButton := 'Avbryt';
  RestartButton := 'Start p� nytt';
  GetUpdateButton := 'Hent oppdatering';
  NewVersionFound:= 'Ny versjon';
  NoNewVersionAvail := 'Ingen ny versjon er tilgjengelig.';
  NewVersionAvail := 'Ny versjon er tilgjengelig for nedlasting.';
  CurrentVersion := 'N�v�rende versjon';
  NoFilesFound := 'Fant ingen filer for oppdateringen';
  NoUpdateOnServer := 'fant ingen oppdatering p� serveren ...';
  CannotConnect := 'Kunne ikke koble til oppdateringsserveren eller ';
  WhatsNew := 'Hva er nytt';
  License := 'Lisensavtale';
  AcceptLicense := 'Jeg godtar';   //Too long for the current radio button width
  NotAcceptLicense := 'Jeg godtar ikke';  //Too long for the current radio button width
  ComponentsAvail := 'Tilgjengelige programkomponenter';
  DownloadingFiles := 'Laster ned filer';
  CurrentProgress := 'Nedlastingsforl�pet for n�v�rende fil';
  TotalProgress := 'Nedlastingsforl�pet for alle filer';
  UpdateComplete := 'Oppdateringen er ferdig ...';
  RestartInfo := 'Klikk Start p� nytt  for � starte det oppdaterte programmet.';
end;

{ TWebUpdateWizardHungarian }

constructor TWebUpdateWizardHungarian.Create(AOwner: TComponent);
begin
  inherited;
  Welcome := 'Kattints az ind�t gombra �s elindul a friss�t�sek keres�se ...';
  StartButton := 'Ind�t';
  NextButton := 'Tov�bb';
  ExitButton := 'Kil�p�s';
  CancelButton := 'M�gsem';
  RestartButton := '�jraind�t';
  GetUpdateButton := 'Friss�t';
  NewVersionFound := '�j verzi�t tal�ltam';
  NewVersion := '�j verzi�';
  NoNewVersionAvail := '�j verzi� nem tal�lhat�.';
  NewVersionAvail := '�j verzi� tal�lhat�.';
  CurrentVersion := 'Aktu�lis verzi�';
  NoFilesFound := 'A friss�t�sben nem tal�lhat� file';
  NoUpdateOnServer := 'nem tal�lhat� friss�t�s a kiszolg�l�n ...';
  CannotConnect := 'Nem tudok kapcsol�dni a friss�t� kiszolg�l�hoz';
  WhatsNew := 'Mi az �jdons�g';
  License := 'Szerz�d�si felt�tel';
  AcceptLicense := 'Elfogadom';
  NotAcceptLicense := 'Visszautas�tom';
  ComponentsAvail := 'Lehets�ges alkalmaz�s kopmponensek';
  DownloadingFiles := '�lom�nyok let�lt�se';
  CurrentProgress := 'Aktu�lis m�velet �llpota';
  TotalProgress := 'Teljes m�velet �llapota';
  UpdateComplete := 'Friss�t�s k�sz ...';
  RestartInfo := 'Kattints az Ujraind�t gombra, hogy elinduljon a friss�tett alkalmaz�s.';
end;


{ TWebUpdateWizardSwedish }

constructor TWebUpdateWizardSwedish.Create(AOwner: TComponent);
begin
  inherited;
    Welcome := 'Tryck p'#229' Start-knappen f'#246'r att leta efter tillg'#228'ngliga uppdateri' +
      'ngar ...';
    StartButton := 'Starta';
    NextButton := 'N'#228'sta';
    ExitButton := 'Avsluta';
    RestartButton := 'Starta om';
    CancelButton := #197'ngra';
    FailedDownload := 'Misslyckades att h'#228'mta uppdateringar';
    GetUpdateButton := 'H'#228'mta uppdatering';
    NewVersionFound := 'Hittat ny version';
    NewVersion := 'Ny version';
    NoNewVersionAvail := 'Ny version saknas.';
    NewVersionAvail := 'Ny version finns';
    CurrentVersion := 'Aktuell version';
    NoFilesFound := 'Hittade inga uppdateringsbara filer';
    NoUpdateOnServer := 'hittade ingen uppdatering p'#229' servern ...';
    CannotConnect := 'Kunde inte f'#229' kontakt med servern eller';
    WhatsNew := 'Nyheter';
    License := 'Licensavtal';
    AcceptLicense := 'Jag accepterar';
    NotAcceptLicense := 'Jag accepterar inte';
    ComponentsAvail := 'Tillg'#228'ngliga applikationskomponenter';
    DownloadingFiles := 'H'#228'mtar filer';
    CurrentProgress := 'P'#229'g'#229'ende filf'#246'rlopp';
    TotalProgress := 'Totalt filf'#246'rlopp';
    UpdateComplete := 'Uppdateringen klar ...';
    RestartInfo := 'Tryck p'#229' "Starta om" f'#246'r att starta den uppdaterade applikatione' +
      'n';
    WhatsNewPopup := 'Visa i anteckningar';
    LicensePopup := 'Visa i Anteckningar';
end;

{ TWebUpdateWizardCzech }

constructor TWebUpdateWizardCzech.Create(AOwner: TComponent);
begin
  inherited;
    Welcome := 'Stiskn�te spustit pro ov��en� existence nov� verze aplikace ...';
    StartButton := 'Spustit';
    NextButton := 'Dal��';
    ExitButton := 'Ukon�it';
    RestartButton := 'Obnovit';
    CancelButton := 'Zru�it';
    FailedDownload := 'Nepoda�ilo se z�skat novou verzi';
    GetUpdateButton := 'Z�skat novou verzi';
    NewVersionFound := 'Nalezena nov� verze';
    NewVersion := 'Nov� verze';
    NoNewVersionAvail := 'Nov� verze nen� k dispozici.';
    NewVersionAvail := 'Nov� verze je k dispozici.';
    CurrentVersion := 'Sou�asn� verze';
    NoFilesFound := 'Nenalezeny soubory nov� verze';
    NoUpdateOnServer := '��dn� nov� verze nenalezena ...';
    CannotConnect := 'Nebylo mo�no se p�ipojit nebo';
    WhatsNew := 'Co je nov�ho';
    License := 'Licen�n� smlouva';
    AcceptLicense := 'Souhlas�m';
    NotAcceptLicense := 'Nesouhlas�m';
    ComponentsAvail := 'Dostupn� sou��sti aplikace';
    DownloadingFiles := 'Stahov�n� soubor�';
    CurrentProgress := 'Stav aktu�ln�ho souboru';
    TotalProgress := 'Stav v�ech soubor�';
    UpdateComplete := 'Sta�en� nov� verze ukon�eno...';
    RestartInfo := 'Stiskn�te obnovit pro aplikov�n� zm�n.,';
    WhatsNewPopup := 'Otev��t v Pozn�mkov�m bloku';
    LicensePopup := 'Otev��t v Pozn�mkov�m bloku';
end;

{ TWebUpdateWizardPolish }

constructor TWebUpdateWizardPolish.Create(AOwner: TComponent);
begin
  inherited;
  Welcome := 'Naci�nij START, aby sprawdzi� dost�pno�� uaktualnie�...';
  StartButton := 'Start';
  NextButton := 'Dalej';
  ExitButton := 'Wyj�cie';
  RestartButton := 'Zako�cz';
  CancelButton := 'Anuluj';
  GetUpdateButton := 'Pobierz';
  NewVersionFound := 'Znaleziono now� wersj�';
  NewVersion := 'Nowa wersja';
  NoNewVersionAvail := 'Nie znaleziono nowszej wersji';
  NewVersionAvail := 'Dost�pna jest nowsza wersja';
  CurrentVersion := 'Aktualna wersja';
  NoFilesFound := 'Nie odnaleziono plik�w na serwerze';
  NoUpdateOnServer := 'brak aktualizacji na serwerze!';
  CannotConnect := 'Brak po��czenia z serwerem lub';
  WhatsNew := 'Co nowego?';
  License := 'Licencja';
  AcceptLicense := 'Akceptuj� licencj�' ;
  NotAcceptLicense := 'Nie akceptuj� licencji';
  ComponentsAvail := 'Dost�pne komponenty';
  DownloadingFiles := 'Pobieranie plik�w';
  CurrentProgress := 'Post�p aktualnego pliku';
  TotalProgress := '��czny post�p';
  FailedDownload := 'Nie uda�o si� pobra� uaktualnienia';
  UpdateComplete := 'Aktualizacja zako�czona !';
  RestartInfo := 'Naci�nij ZAKO�CZ, aby uruchomi� now� wersj� aplikacji';
end;

{ TWebUpdateWizardCatalan }

constructor TWebUpdateWizardCatalan.Create(AOwner: TComponent);
begin
  inherited;
  Welcome := 'Premi Inicia para cercar actualitzacions disponibles de l''aplicaci� ...';
  StartButton := 'Inicia';
  NextButton := 'Seg�ent';
  ExitButton := 'Surt';
  CancelButton := 'Cancel�la';
  RestartButton := 'Reinicia';
  GetUpdateButton := 'Obten actualitzaci�';
  NewVersionFound := 'S''ha trobat una nova versi�';
  NewVersion := 'Nova versi�';
  NoNewVersionAvail := 'No hi ha cap nova versi� disponible.';
  NewVersionAvail := 'Hi ha una nova versi� disponible.';
  CurrentVersion := 'Versi� actual';
  NoFilesFound := 'No s''han trobat els fitxers per actualitzar';
  NoUpdateOnServer := 'no s''ha trobat cap nova actualitzaci� al servidor ...';
  CannotConnect := 'No es pot establir la connexi� amb el servidor d''actualitzacions o ';
  WhatsNew := 'Qu� hi ha de nou';
  License := 'Acord de llic�ncia';
  AcceptLicense := 'Accepto';
  NotAcceptLicense := 'No accepto';
  ComponentsAvail := 'Components de l''aplicaci� disponibles';
  DownloadingFiles := 'Descarregant fitxers';
  CurrentProgress := 'Progr�s del fitxer actual';
  TotalProgress := 'Progr�s total';
  UpdateComplete := 'Actualitzaci� completada ...';
  RestartInfo := 'Premi Reiniciar per executar l''aplicaci� actualitzada.';
  FailedDownload := 'No s''ha pogut descarregar l''actualitzaci�';
  LicensePopup := 'Veure amb Notepad';
  WhatsNewPopup := 'Veure amb Notepad';
end;


end.
