#!/usr/bin/env zsh

# Override the j function defined in /usr/share/autojump/autojump.zsh
# to avoid printing the destination directory to the terminal, which
# is superfluous since my prompt shows the current directory in
# absolute form.
j() {
    if [[ ${1} == -* ]] && [[ ${1} != "--" ]]; then
        autojump ${@}
        return
    fi

    setopt localoptions noautonamedirs
    local output="$(autojump ${@})"
    if [[ -d "${output}" ]]; then
        cd "${output}"                                                                                                                                                                                                                         
    else                                                                                                                                                                                                                                       
        echo "autojump: directory '${@}' not found"                                                                                                                                                                                            
        echo "\n${output}\n"                                                                                                                                                                                                                   
        echo "Try \`autojump --help\` for more information."                                                                                                                                                                                   
        false                                                                                                                                                                                                                                  
    fi                                                                                                                                                                                                                                         
}
