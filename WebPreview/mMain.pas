// -----------------------------------------------------------------------------
// WebƒvƒŒƒrƒ…[
//
// Copyright (c) Kuro. All Rights Reserved.
// e-mail: info@haijin-boys.com
// www:    https://www.haijin-boys.com/
// -----------------------------------------------------------------------------

unit mMain;

interface

uses
{$IF CompilerVersion > 22.9}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.Menus, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.OleCtrls,
{$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, ComCtrls, ExtCtrls, OleCtrls,
{$IFEND}
  SHDocVw, UWBDragDropContainer, UCustomDropTarget, mPerMonitorDpi;

type
  TMainForm = class(TScaledForm, IDropHandler)
    PopupMenu: TPopupMenu;
    BackMenuItem: TMenuItem;
    ForwardMenuItem: TMenuItem;
    N1: TMenuItem;
    RefreshMenuItem: TMenuItem;
    N2: TMenuItem;
    PropMenuItem: TMenuItem;
    WebBrowser: TWebBrowser;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PopupMenuPopup(Sender: TObject);
    procedure BackMenuItemClick(Sender: TObject);
    procedure ForwardMenuItemClick(Sender: TObject);
    procedure RefreshMenuItemClick(Sender: TObject);
    procedure PropMenuItemClick(Sender: TObject);
    procedure WebBrowserCommandStateChange(ASender: TObject; Command: Integer;
      Enable: WordBool);
    procedure WebBrowserNavigateComplete2(ASender: TObject;
      const pDisp: IDispatch; const URL: OleVariant);
  private
    { Private éŒ¾ }
    FEditor: THandle;
    FBarPos: Integer;
    FAutoDisplay: Boolean;
    FModeList: TStringList;
    FUpdateWebPreview: Boolean;
    FWBContainer: TWBDragDropContainer;
    FBackEnabled: Boolean;
    FForwardEnabled: Boolean;
    FFileName: string;
    FPoint: TPoint;
    procedure ReadIni;
    procedure WriteIni;
  protected
    { Protected éŒ¾ }
    procedure HandleText(const Text: string);
    procedure HandleFiles(const Files: TStrings);
  public
    { Public éŒ¾ }
    procedure WebPreviewAll;
    procedure SetScale(const Value: Integer);
    function SetProperties: Boolean;
    property BarPos: Integer read FBarPos write FBarPos;
    property AutoDisplay: Boolean read FAutoDisplay write FAutoDisplay;
    property ModeList: TStringList read FModeList write FModeList;
    property UpdateWebPreview: Boolean read FUpdateWebPreview write FUpdateWebPreview;
    property Editor: THandle read FEditor write FEditor;
  end;

var
  MainForm: TMainForm;

implementation

uses
{$IF CompilerVersion > 22.9}
  System.Types, Winapi.ActiveX, System.IniFiles,
{$ELSE}
  Types, ActiveX, IniFiles,
{$IFEND}
  MSHTML, mCommon, mPlugin, mProp;

{$R *.dfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  TScaledForm.DefaultFont.Assign(Font);
  FEditor := ParentWindow;
  FModeList := TStringList.Create;
  FModeList.CaseSensitive := False;
  FUpdateWebPreview := False;
  FFileName := '';
  FPoint.X := 0;
  FPoint.Y := 0;
  ReadIni;
  OleInitialize(nil);
  FWBContainer := TWBDragDropContainer.Create(WebBrowser);
  FWBContainer.DropTarget := TCustomDropTarget.Create(Self);
  FWBContainer.UseCustomCtxMenu := True;
  WebBrowser.Navigate('about:blank');
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  WriteIni;
  FWBContainer.DropTarget := nil;
  FreeAndNil(FWBContainer);
  OleUninitialize;
  if Assigned(FModeList) then
    FreeAndNil(FModeList);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  //
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  //
end;

procedure TMainForm.PopupMenuPopup(Sender: TObject);
begin
  BackMenuItem.Enabled := FBackEnabled;
  ForwardMenuItem.Enabled := FForwardEnabled;
end;

procedure TMainForm.BackMenuItemClick(Sender: TObject);
begin
  if FBackEnabled then
    WebBrowser.GoBack;
end;

procedure TMainForm.ForwardMenuItemClick(Sender: TObject);
begin
  if FForwardEnabled then
    WebBrowser.GoForward;
end;

procedure TMainForm.RefreshMenuItemClick(Sender: TObject);
begin
  WebPreviewAll;
end;

procedure TMainForm.PropMenuItemClick(Sender: TObject);
begin
  SetProperties;
end;

procedure TMainForm.WebBrowserCommandStateChange(ASender: TObject;
  Command: Integer; Enable: WordBool);
begin
  case Command of
    CSC_NAVIGATEFORWARD:
      FForwardEnabled := Enable;
    CSC_NAVIGATEBACK:
      FBackEnabled := Enable;
  end;
end;

procedure TMainForm.WebBrowserNavigateComplete2(ASender: TObject;
  const pDisp: IDispatch; const URL: OleVariant);
begin
  OLEVariant(WebBrowser.Document as IHTMLDocument2).ParentWindow.Scroll(FPoint.X, FPoint.Y);
end;

procedure TMainForm.ReadIni;
var
  S: string;
begin
  if not GetIniFileName(S) then
    Exit;
  with TMemIniFile.Create(S, TEncoding.UTF8) do
    try
      with TScaledForm.DefaultFont do
        if ValueExists('MainForm', 'FontName') then
        begin
          Name := ReadString('MainForm', 'FontName', Name);
          Size := ReadInteger('MainForm', 'FontSize', Size);
        end
        else if CheckWin32Version(6, 2) then
          Assign(Screen.IconFont);
    finally
      Free;
    end;
end;

procedure TMainForm.WriteIni;
var
  S: string;
begin
  if FIniFailed or (not GetIniFileName(S)) then
    Exit;
  try
    with TMemIniFile.Create(S, TEncoding.UTF8) do
      try
        WriteInteger('WebPreview', 'CustomBarPos', FBarPos);
        WriteBool('WebPreview', 'AutoDisplay', FAutoDisplay);
        WriteString('WebPreview', 'ModeList', FModeList.CommaText);
        UpdateFile;
      finally
        Free;
      end;
  except
    FIniFailed := True;
  end;
end;

procedure TMainForm.HandleText(const Text: string);
begin
  //
end;

procedure TMainForm.HandleFiles(const Files: TStrings);
var
  I: Integer;
  S: string;
begin
  for I := 0 to Files.Count - 1 do
  begin
    S := Files[I];
    Editor_LoadFile(FEditor, True, @S[1]);
  end;
end;

procedure TMainForm.WebPreviewAll;
var
  S: array [0 .. MAX_PATH] of Char;
begin
  Editor_Info(FEditor, MI_GET_FILE_NAME, LPARAM(@S));
  if S <> '' then
  begin
    if SameFileName(S, FFileName) then
    begin
      FPoint.X := OLEVariant(WebBrowser.Document as IHTMLDocument2).body.scrollLeft;
      FPoint.Y := OLEVariant(WebBrowser.Document as IHTMLDocument2).body.scrollTop;
    end
    else
    begin
      FPoint.X := 0;
      FPoint.Y := 0;
    end;
    WebBrowser.Silent := True;
    WebBrowser.Navigate(S, navNoReadFromCache or navNoWriteToCache);
    FFileName := S;
  end;
end;

procedure TMainForm.SetScale(const Value: Integer);
var
  P: Integer;
begin
  P := PixelsPerInch;
  PixelsPerInch := Value;
  with Font do
    Height := MulDiv(Height, Self.PixelsPerInch, P);
end;

function TMainForm.SetProperties: Boolean;
var
  S, P: PChar;
  Len: Cardinal;
  Modes: TStrings;
begin
  Result := False;
  Modes := TStringList.Create;
  try
    Len := Editor_EnumMode(FEditor, nil, 0);
    S := StrAlloc(Len);
    try
      Editor_EnumMode(FEditor, S, Len);
      P := S;
      while P^ <> #0 do
      begin
        Modes.Add(P);
        Inc(P, StrLen(P) + 1);
      end;
    finally
      StrDispose(S);
    end;
    if Prop(Self, FBarPos, FAutoDisplay, FModeList, Modes) then
    begin
      WriteIni;
      FUpdateWebPreview := True;
      Result := True;
    end;
  finally
    Modes.Free;
  end;
end;

end.
