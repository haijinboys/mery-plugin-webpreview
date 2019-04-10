// -----------------------------------------------------------------------------
// Webプレビュー
//
// Copyright (c) Kuro. All Rights Reserved.
// e-mail: info@haijin-boys.com
// www:    https://www.haijin-boys.com/
// -----------------------------------------------------------------------------

unit mWebPreview;

interface

uses
{$IF CompilerVersion > 22.9}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
{$ELSE}
  Windows, Messages, SysUtils, Classes,
{$IFEND}
  mCommon, mMain, mFrame, mPlugin;

resourcestring
  SName = 'Webプレビュー';
  SVersion = '2.3.3';

type
  TWebPreviewFrame = class(TFrame)
  private
    { Private 宣言 }
    FForm: TMainForm;
    FClientID: Cardinal;
    FBarPos: Integer;
    FAutoDisplay: Boolean;
    FModeList: TStringList;
    FMode: string;
    function QueryProperties: Boolean;
    function SetProperties: Boolean;
    function PreTranslateMessage(hwnd: HWND; var Msg: tagMSG): Boolean;
    procedure OpenCustomBar;
    procedure CloseCustomBar;
    procedure CustomBarClosed;
  protected
    { Protected 宣言 }
  public
    { Public 宣言 }
    constructor Create;
    destructor Destroy; override;
    procedure OnIdle;
    procedure OnCommand(hwnd: HWND); override;
    function QueryStatus(hwnd: HWND; pbChecked: PBOOL): BOOL; override;
    procedure OnEvents(hwnd: HWND; nEvent: Cardinal; lParam: LPARAM); override;
    function PluginProc(hwnd: HWND; nMsg: Cardinal; wParam: WPARAM; lParam: LPARAM): LRESULT; override;
  end;

implementation

uses
{$IF CompilerVersion > 22.9}
  System.IniFiles;
{$ELSE}
  IniFiles;
{$IFEND}

{ TWebPreviewFrame }

constructor TWebPreviewFrame.Create;
begin
  FModeList := TStringList.Create;
  FModeList.CaseSensitive := False;
end;

destructor TWebPreviewFrame.Destroy;
begin
  if Assigned(FModeList) then
    FreeAndNil(FModeList);
  inherited;
end;

function TWebPreviewFrame.QueryProperties: Boolean;
begin
  Result := True;
end;

function TWebPreviewFrame.SetProperties: Boolean;
begin
  if FForm <> nil then
    Result := FForm.SetProperties
  else
  begin
    with TMainForm.CreateParented(Handle) do
      try
        BarPos := FBarPos;
        AutoDisplay := FAutoDisplay;
        ModeList.Assign(FModeList);
        Result := SetProperties;
        if Result then
        begin
          FBarPos := BarPos;
          FAutoDisplay := AutoDisplay;
          FModeList.Assign(ModeList);
        end;
      finally
        Free;
      end;
  end;
end;

function TWebPreviewFrame.PreTranslateMessage(hwnd: HWND; var Msg: tagMSG): Boolean;
var
  Ctrl, Shift: Boolean;
begin
  Result := False;
  if (FForm <> nil) and (FForm.WebBrowser.Handle = GetFocus) then
  begin
    if Msg.message = WM_KEYDOWN then
    begin
      Ctrl := GetKeyState(VK_CONTROL) < 0;
      Shift := GetKeyState(VK_SHIFT) < 0;
      if not Ctrl then
      begin
        if Msg.wParam = VK_ESCAPE then
        begin
          if not Shift then
          begin
            Editor_ExecCommand(hwnd, MEID_WINDOW_ACTIVE_PANE);
            Result := True;
            Exit;
          end;
        end;
      end;
      if ((Msg.wParam >= VK_PRIOR) and (Msg.wParam <= VK_DELETE)) or (Msg.wParam = VK_TAB) or (Msg.wParam = VK_BACK) or (Msg.wParam = VK_ESCAPE) or (Msg.wParam = VK_RETURN) then
      begin
        SendMessage(GetFocus, Msg.message, Msg.wParam, Msg.lParam);
        Result := True;
        Exit;
      end;
    end;
    if IsDialogMessage(FForm.Handle, Msg) then
      Result := True;
  end;
end;

procedure TWebPreviewFrame.OpenCustomBar;
var
  Info: TCustomBarInfo;
begin
  if FForm = nil then
  begin
    FForm := TMainForm.CreateParented(Handle);
    with FForm do
    begin
      Left := 0;
      Top := 0;
      Visible := True;
      BarPos := FBarPos;
      AutoDisplay := FAutoDisplay;
      ModeList.Assign(FModeList);
    end;
    with Info do
    begin
      cbSize := SizeOf(Info);
      hwndClient := FForm.Handle;
      pszTitle := PChar(SName);
      iPos := FBarPos;
    end;
    FClientID := Editor_CustomBarOpen(Handle, @Info);
    if FClientID = 0 then
      CustomBarClosed
    else
      with FForm do
        UpdateWebPreview := True;
  end;
end;

procedure TWebPreviewFrame.CloseCustomBar;
begin
  if FForm <> nil then
  begin
    Editor_CustomBarClose(Handle, FClientID);
    CustomBarClosed;
  end;
end;

procedure TWebPreviewFrame.CustomBarClosed;
begin
  if FForm <> nil then
    FreeAndNil(FForm);
  FClientID := 0;
end;

procedure TWebPreviewFrame.OnIdle;
begin
  if FForm <> nil then
  begin
    with FForm do
    begin
      if UpdateWebPreview then
        WebPreviewAll;
      if UpdateWebPreview then
      begin
        if FBarPos <> BarPos then
        begin
          FBarPos := BarPos;
          CloseCustomBar;
          OpenCustomBar;
          Exit;
        end;
        FAutoDisplay := AutoDisplay;
        FModeList.Assign(ModeList);
      end;
      UpdateWebPreview := False;
    end;
  end;
end;

procedure TWebPreviewFrame.OnCommand(hwnd: HWND);
begin
  if FForm = nil then
    OpenCustomBar
  else
    CloseCustomBar;
end;

function TWebPreviewFrame.QueryStatus(hwnd: HWND; pbChecked: PBOOL): BOOL;
begin
  pbChecked^ := FForm <> nil;
  Result := True;
end;

procedure TWebPreviewFrame.OnEvents(hwnd: HWND; nEvent: Cardinal; lParam: LPARAM);
var
  S: string;
  Info: TCustomBarCloseInfo;
  Mode: array [0 .. MAX_MODE_NAME - 1] of Char;
begin
  if (nEvent and EVENT_CREATE_FRAME) <> 0 then
  begin
    if not GetIniFileName(S) then
      Exit;
    with TMemIniFile.Create(S, TEncoding.UTF8) do
      try
        FBarPos := ReadInteger('WebPreview', 'CustomBarPos', CUSTOM_BAR_BOTTOM);
        FAutoDisplay := ReadBool('WebPreview', 'AutoDisplay', False);
        FModeList.CommaText := ReadString('WebPreview', 'ModeList', 'HTML');
      finally
        Free;
      end;
    Mode[0] := #0;
    Editor_GetMode(hwnd, @Mode);
    FMode := Mode;
    if FAutoDisplay and (FModeList.IndexOf(Mode) > -1) then
      OnCommand(hwnd);
  end;
  if (nEvent and EVENT_CLOSE_FRAME) <> 0 then
    CloseCustomBar;
  if (nEvent and (EVENT_FILE_OPENED or EVENT_MODE_CHANGED or EVENT_DOC_SEL_CHANGED)) <> 0 then
  begin
    if FAutoDisplay then
    begin
      Mode[0] := #0;
      Editor_GetMode(hwnd, @Mode);
      if not SameText(Mode, FMode) then
      begin
        if FModeList.IndexOf(Mode) > -1 then
          OpenCustomBar
        else
          CloseCustomBar;
        FMode := Mode;
      end;
    end;
  end;
  if (nEvent and (EVENT_MODE_CHANGED or EVENT_DOC_SEL_CHANGED)) <> 0 then
  begin
    if FForm <> nil then
      with FForm do
        UpdateWebPreview := True;
  end;
  if (nEvent and EVENT_CUSTOM_BAR_CLOSING) <> 0 then
  begin
    Info := PCustomBarCloseInfo(lParam)^;
    if Info.nID = FClientID then
    begin
      if FForm <> nil then
        FreeAndNil(FForm);
    end;
  end;
  if (nEvent and EVENT_CUSTOM_BAR_CLOSED) <> 0 then
  begin
    Info := PCustomBarCloseInfo(lParam)^;
    if Info.nID = FClientID then
      CustomBarClosed;
  end;
  if (nEvent and EVENT_FILE_SAVED) <> 0 then
  begin
    if FForm <> nil then
      FForm.UpdateWebPreview := True;
  end;
  if (nEvent and EVENT_IDLE) <> 0 then
    OnIdle;
  if (nEvent and EVENT_DPI_CHANGED) <> 0 then
  begin
    if FForm <> nil then
      FForm.SetScale(lParam);
  end;
end;

function TWebPreviewFrame.PluginProc(hwnd: HWND; nMsg: Cardinal; wParam: WPARAM; lParam: LPARAM): LRESULT;
begin
  Result := 0;
  case nMsg of
    MP_QUERY_PROPERTIES:
      Result := LRESULT(QueryProperties);
    MP_SET_PROPERTIES:
      Result := LRESULT(SetProperties);
    MP_PRE_TRANSLATE_MSG:
      Result := LRESULT(PreTranslateMessage(hwnd, PMsg(lParam)^));
  end;
end;

end.
