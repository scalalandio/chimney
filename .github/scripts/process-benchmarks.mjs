import { argv } from 'node:process'
import { promises as fs } from 'node:fs'
import { inspect } from 'node:util'

const metaFileName = 'meta.json'
const triggerLabel = 'benchmark'

const githubContext = JSON.parse(argv[2])
const gitDescribe = argv[3]

const metadata = JSON.parse(await fs.readFile(metaFileName, 'utf8'))

if (githubContext.event_name === 'push') {
    const { event, event_name, ref } = githubContext

    const { head_commit, compare } = event

    const entry = {
        file: `${githubContext.sha}.json`,
        sha: event.after,
        event: event_name,
        describe: gitDescribe,
        ref,
        author: head_commit.author.username,
        link: compare,
        timestamp: head_commit.timestamp,
        prevCommit: event.before,
    }

    metadata.push(entry)

    await fs.writeFile(metaFileName, JSON.stringify(metadata, null, 2))

    console.log('added entry to metadata:', inspect(entry, {showHidden: false, depth: null, colors: true}))
}

if (githubContext.event_name === 'pull_request') {
    const { event, event_name, ref } = githubContext

    if (event.action === 'labeled' && event.label.name === triggerLabel) { // fire for labelling as 'benchmark'
        const { label, pull_request } = event

        const entry = {
            file: `${githubContext.sha}.json`,
            sha: githubContext.sha,
            event: event_name,
            describe: gitDescribe,
            ref,
            author: pull_request.head.user.login,
            link: pull_request._links.html.href,
            timestamp: pull_request.updated_at,
            prNumber: event.number,
            sourceBranch: pull_request.head.user.login + '/' + pull_request.head.repo.name + '/' + pull_request.head.ref,
            label: label.name,
            prTitle: pull_request.title,
        }

        metadata.push(entry)

        await fs.writeFile(metaFileName, JSON.stringify(metadata, null, 2))

        console.log('added entry to metadata:', inspect(entry, {showHidden: false, depth: null, colors: true}))
    }

    if (event.action === 'synchronize') {
        const { pull_request } = event

        const entry = {
            file: `${githubContext.sha}.json`,
            sha: githubContext.sha,
            event: event_name,
            describe: gitDescribe,
            ref,
            author: pull_request.head.user.login,
            link: pull_request._links.html.href,
            timestamp: pull_request.updated_at,
            prNumber: event.number,
            sourceBranch: pull_request.head.user.login + '/' + pull_request.head.repo.name + '/' + pull_request.head.ref,
            prTitle: pull_request.title,
        }

        metadata.push(entry)

        await fs.writeFile(metaFileName, JSON.stringify(metadata, null, 2))

        console.log('added entry to metadata:', inspect(entry, {showHidden: false, depth: null, colors: true}))
    }
}

