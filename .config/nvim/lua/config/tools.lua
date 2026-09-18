-- Two small conveniences that used to be plugins here (vim-gh-line, go.nvim).

local M = {}

--- Open the current line on the project's forge (GitHub/GitLab/...) in a browser.
function M.browse_line()
  local root = vim.fs.root(0, '.git')
  local file = vim.api.nvim_buf_get_name(0)
  if not root or file == '' then
    return vim.notify('Not inside a git work tree', vim.log.levels.WARN)
  end

  local function git(...)
    local res = vim.system({ 'git', '-C', root, ... }, { text = true }):wait()
    return res.code == 0 and vim.trim(res.stdout) or nil
  end

  local remote = git('remote', 'get-url', 'origin')
  local rev = git('rev-parse', 'HEAD')
  if not remote or not rev then
    return vim.notify('No `origin` remote found', vim.log.levels.WARN)
  end

  -- git@host:owner/repo.git | https://host/owner/repo.git -> https://host/owner/repo
  local host, path = remote:match('^git@([^:]+):(.+)$')
  if not host then
    host, path = remote:match('^https?://([^/]+)/(.+)$')
  end
  if not host then
    return vim.notify('Cannot parse remote: ' .. remote, vim.log.levels.WARN)
  end
  path = path:gsub('%.git$', '')

  local infix = host:find('gitlab') and '/-/blob/' or '/blob/'
  local relative = file:sub(#root + 2)
  local url =
    ('https://%s/%s%s%s/%s#L%d'):format(host, path, infix, rev, relative, vim.fn.line('.'))

  vim.ui.open(url)
  vim.notify(url)
end

--- Run the project's test suite in a terminal split.
local runners = {
  go = { 'go', 'test', './...' },
  rust = { 'cargo', 'test' },
  python = { 'pytest' },
  javascript = { 'npm', 'test' },
  typescript = { 'npm', 'test' },
}

function M.test()
  local cmd = runners[vim.bo.filetype]
  if not cmd then
    return vim.notify('No test runner for filetype: ' .. vim.bo.filetype, vim.log.levels.WARN)
  end
  if vim.bo.modified then
    vim.cmd.write()
  end
  vim.cmd('botright 15split')
  vim.cmd.terminal(table.concat(cmd, ' '))
  vim.cmd.startinsert()
end

return M
