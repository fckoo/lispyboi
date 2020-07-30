#include "platform.hpp"

#include <cstdlib>

#include <unistd.h> // readlink
#include <limits.h> // PATH_MAX

std::filesystem::path plat::get_working_directory(std::error_code &error)
{
    return std::filesystem::current_path(error);
}

void plat::change_directory(const std::filesystem::path &new_dir, std::error_code &error)
{
    std::filesystem::current_path(new_dir, error);
}

std::filesystem::path plat::get_executable_path()
{
    static char path[PATH_MAX]{0};
    static auto len = readlink("/proc/self/exe", path, PATH_MAX);
    if (len > 0) {
        return std::string(path, len);
    }
    return std::string();
}

void plat::clear_console()
{
    std::system("clear");
}
